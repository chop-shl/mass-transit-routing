# =============================================================================
# 04_detailed_routes.R   (OPTIONAL)
#
# WHAT THIS DOES:
#   Calculates the actual walk + transit path each patient takes to their
#   closest hospital, then maps all the routes. Slow, so it's optional.
#
#   Routes are computed in batches and each batch is saved immediately. If the
#   session crashes at batch 5, re-running resumes at batch 6 (finished batches
#   are skipped). This mirrors your original resume-on-crash approach.
#
# WHY YOU MIGHT GET FEWER ROUTES THAN patient_access ROWS:
#   detailed_itineraries needs a real transit schedule for the departure DATE.
#   If the date is outside the GTFS calendar window, it can't build transit
#   trips and returns few or no routes -- even though the travel-time matrix
#   gave you numbers. Run check_gtfs_dates() to find a valid date.
#
# PHI NOTE: routes start at patient homes. Keep on a PHI-approved machine.
#
# HOW TO RUN (after 00, 02, 03):
#   source("scripts/04_detailed_routes.R")
#   # the map prints automatically; type  route_map  to bring it back
# =============================================================================

library(r5r)
library(sf)
library(dplyr)
library(purrr)
library(mapview)

# --- Helper: what dates do your GTFS feeds cover? ----------------------------
# Run check_gtfs_dates() if you see a "less than X% of transit services" warning.
check_gtfs_dates <- function() {
  zips <- list.files(network_dir, pattern = "\\.zip$", full.names = TRUE)
  if (length(zips) == 0) { message("No GTFS zips in ", network_dir); return(invisible()) }

  for (z in zips) {
    inside <- unzip(z, list = TRUE)$Name
    if ("calendar.txt" %in% inside) {
      cal <- read.csv(unz(z, "calendar.txt"), colClasses = "character")
      if (all(c("start_date", "end_date") %in% names(cal))) {
        cat(basename(z), "-> covers",
            format(min(as.Date(cal$start_date, "%Y%m%d")), "%Y-%m-%d"), "to",
            format(max(as.Date(cal$end_date,   "%Y%m%d")), "%Y-%m-%d"), "\n")
        next
      }
    }
    if ("calendar_dates.txt" %in% inside) {
      cd <- read.csv(unz(z, "calendar_dates.txt"), colClasses = "character")
      if ("date" %in% names(cd)) {
        d <- as.Date(cd$date, "%Y%m%d")
        cat(basename(z), "-> dates",
            format(min(d), "%Y-%m-%d"), "to", format(max(d), "%Y-%m-%d"), "\n")
        next
      }
    }
    cat(basename(z), "-> no calendar found\n")
  }
  cat("\nPick a date inside ALL ranges above, then update route_departure\n",
      "and departure_times in 00_settings.R. Delete the cache files and re-run.\n")
}

# --- 1. Build origin/destination pairs (patient -> closest hospital) ---------
od_pairs <- patient_access %>%
  select(patient_id, closest_hospital) %>%
  inner_join(patients_all %>% select(id, lon, lat),
             by = c("patient_id" = "id")) %>%
  inner_join(hospitals %>% select(id, lon, lat),
             by = c("closest_hospital" = "id"),
             suffix = c("_origin", "_dest"))

origins_di      <- od_pairs %>% transmute(id = patient_id,       lon = lon_origin, lat = lat_origin)
destinations_di <- od_pairs %>% transmute(id = closest_hospital, lon = lon_dest,   lat = lat_dest)

cat("OD pairs prepared:", nrow(od_pairs), "routes to compute.\n")

# --- 2. Compute routes in resumable batches ----------------------------------
if (file.exists(routes_cache)) {
  cat("Loading saved routes.\n")
  routes_raw <- readRDS(routes_cache)
} else {
  dir.create(batch_dir, showWarnings = FALSE, recursive = TRUE)

  n <- nrow(origins_di)
  batch_indices <- split(seq_len(n), ceiling(seq_len(n) / route_batch_size))
  cat("Total batches:", length(batch_indices), "\n")

  for (i in seq_along(batch_indices)) {
    batch_file <- file.path(batch_dir, sprintf("batch_%04d.rds", i))
    if (file.exists(batch_file)) { cat("Batch", i, "already saved - skipping\n"); next }

    idx <- batch_indices[[i]]
    cat("Running batch", i, "of", length(batch_indices),
        "| rows", min(idx), "-", max(idx), "\n")

    result <- tryCatch(
      detailed_itineraries(
        r5r_network,
        origins            = origins_di[idx, ],
        destinations       = destinations_di[idx, ],
        mode               = c("WALK", "TRANSIT"),
        departure_datetime = route_departure,
        max_walk_time      = max_walk_time,
        walk_speed         = walk_speed,
        max_rides          = max_rides,
        shortest_path      = TRUE
      ),
      error = function(e) { cat("  ERROR in batch", i, ":", conditionMessage(e), "\n"); NULL }
    )

    if (!is.null(result) && nrow(result) > 0) {
      saveRDS(result, batch_file)
      cat("  saved:", basename(batch_file), "\n")
    } else {
      cat("  no result for batch", i, "\n")
    }
  }

  batch_files <- list.files(batch_dir, pattern = "^batch_.*\\.rds$", full.names = TRUE)
  cat("Combining", length(batch_files), "batch files...\n")
  routes_raw <- map_dfr(batch_files, readRDS)
  saveRDS(routes_raw, routes_cache)
  cat("Saved combined routes to", routes_cache, "\n")
}

# --- 3. Sanity check ----------------------------------------------------------
if (is.null(routes_raw) || nrow(routes_raw) == 0) {
  stop("No routes returned. The departure date is likely outside the GTFS\n",
       "calendar. Run check_gtfs_dates(), update the dates in 00_settings.R,\n",
       "delete ", routes_cache, " and ", batch_dir, ", then re-run.")
}

n_routed <- length(unique(routes_raw$from_id))
cat(n_routed, "of", nrow(origins_di), "patients have a route.\n")
if (n_routed < nrow(origins_di) * 0.5) {
  cat("WARNING: fewer than half got routes. Likely a GTFS date problem.\n",
      "         Run check_gtfs_dates() to check the valid window.\n")
}

# --- 4. Combine each patient's legs into one line ----------------------------
routes_patient <- routes_raw %>%
  group_by(from_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  rename(patient_id = from_id) %>%
  left_join(
    patient_access %>% select(patient_id, closest_hospital,
                              closest_avg_tt, closest_med_tt,
                              closest_min_tt, closest_max_tt),
    by = "patient_id"
  ) %>%
  st_set_crs(4326)

# --- 5. Map it (thick, opaque lines so they're actually visible) -------------
route_map <- mapview(
  routes_patient,
  zcol       = "closest_hospital",
  layer.name = "Patient routes",
  lwd        = 3,
  alpha      = 1
) +
  mapview(
    hospitals_sf,
    col.regions = "black",
    color       = "white",
    cex         = 8,
    layer.name  = "Hospitals"
  )

print(route_map)
cat("\nMap is in your Viewer pane. Type  route_map  to bring it back.\n")
