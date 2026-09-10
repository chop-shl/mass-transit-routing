################################################################################
#                           05_figures_tables.R
#
#   Table 1a   computational cost                       (Results 3.1)
#   Table 1b   routing success, tiers, composition      (Results 3.1, 3.4)
#   Figure     per-origin temporal range, CHOP_PHL      (Results 3.2)
#   Map A      facilities reachable by transit          (Results 3.1, 3.3)
#   Map C      where the appointment hour changes band  (Results 3.2)
#
# Everything writes to outputs/<run_id>/manuscript/. Composite assembly is
# done separately.
#
# Bands are <=30 / 30-60 / >60 and are computed here from BANDS, not read from
# 04's median_burden, which was built on cfg$measures$burden_thresholds and
# may still be the four-threshold scheme. One definition, used by tables and
# figures alike.
################################################################################

source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(jsonlite)
library(sf)
library(nhdplusTools)

tables_dir <- file.path(cfg$paths$outputs, cfg$run_id, "tables")
out_dir    <- file.path(cfg$paths$outputs, cfg$run_id, "manuscript")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

od_hour      <- readRDS(file.path(tables_dir, "od_hour.rds"))
od_pair      <- readRDS(file.path(tables_dir, "od_pair.rds"))
origin_level <- readRDS(file.path(tables_dir, "origin_level.rds"))
od_instant   <- readRDS(file.path(tables_dir, "od_instant.rds"))
telemetry    <- readRDS(file.path(cfg$paths$outputs, cfg$run_id, "run_telemetry.rds"))
manifest     <- read_json(file.path(cfg$paths$network, "network_manifest.json"))


# SETTINGS
# ------------------------------------------------------------------------------

PRIMARY    <- cfg$scenarios$scenario_label[cfg$scenarios$is_primary]
FOCUS_SITE <- "CHOP_PHL"      # only site with enough reachable origins
n_fac      <- nrow(cfg$facilities)

BANDS      <- cfg$measures$burden_thresholds
BAND_LABS  <- c(paste0("<=", BANDS[1]),
                paste0(BANDS[-length(BANDS)], "-", BANDS[-1]),
                paste0(">", BANDS[length(BANDS)]))

MIN_REACH  <- 100L            # instants (of 240) an origin must be reachable at

CRS_MAP        <- 26918       # UTM 18N (NAD83); metres, so buffers are literal
VIEW_BUFFER_KM <- 12
COUNTY_SHP     <- NULL        # local .shp path, or NULL to fetch via tigris
MIN_WATER_KM2  <- 15          # NHD areas below this are ponds, not orientation
SHOW_ROUTES    <- TRUE


# PALETTE
# ------------------------------------------------------------------------------
#   cool low-contrast   context — water, bus, rail
#   neutral grey        ABSENCE — unreachable, zero facilities reachable
#   gold to indigo      ACCESS — better is darker AND a different hue
#   magenta             burden tail (>60 band), continuous instability ramp
#
# The origin-range figure uses the same gold/indigo pair as Map C, so the two
# read as one story when composited: gold is the general case, indigo is the
# temporal signal.

pal <- list(
  land     = "#faf8f5",
  water    = "#8a969b",
  bus      = "#a9c0dd",
  rail     = "#4f74a8",
  border   = "#e0dad1",

  none     = "#c9c6c0",
  access   = c("0" = "#c9c6c0", "1" = "#f0a43c", "2" = "#3b2f6b"),
  band     = c("<=30" = "#3b2f6b", "30-60" = "#f0a43c", ">60" = "#b5306e"),

  stable   = "#f0a43c",
  signal   = "#3b2f6b",
  burden   = c("#f7dcb0", "#f0a43c", "#b5306e", "#6d1440"),

  facility = "#b03a2e",
  halo     = "#ffffff",
  halo2    = "#ebf2f5"
)

# pal$band is keyed by the literal three-band labels; re-key it from BAND_LABS
# so a change to cfg$measures$burden_thresholds does not silently produce an
# all-grey map. Three bands is what the palette is designed for.
stopifnot(length(BAND_LABS) == length(pal$band))
band_cols <- c(setNames(unname(pal$band), BAND_LABS), "unreachable" = pal$none)


# HELPERS
# ------------------------------------------------------------------------------

band_of <- function(t) {
  out <- as.character(cut(t, c(0, BANDS, Inf), labels = BAND_LABS, right = TRUE))
  out[is.na(t)] <- "unreachable"
  factor(out, levels = c(BAND_LABS, "unreachable"))
}

# Manifest fields written by older versions of 01 may be absent; a NULL here
# makes sprintf() return character(0) and takes the whole table down with it.
mf <- function(x, default = "NA") {
  if (is.null(x) || length(x) == 0) default else as.character(x)[1]
}

theme_plot <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title    = element_text(face = "bold", size = base_size + 1),
          plot.subtitle = element_text(colour = "grey35"),
          legend.position = "bottom")
}

theme_map <- function() {
  theme_void(11) +
    theme(plot.title    = element_text(face = "bold", hjust = 0),
          plot.subtitle = element_text(colour = "grey35", hjust = 0),
          legend.position = "bottom",
          strip.text = element_text(face = "bold"),
          panel.border = element_rect(colour = pal$water, fill = NA, linewidth = 0.5))
}

save_fig <- function(p, name, w, h, dpi = 220) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = dpi, bg = "white")
  invisible(p)
}


################################################################################
# TABLE 1a  computational cost
################################################################################
# Results 3.1 treats compute as a finding: a full origin-destination sweep at
# this resolution is tractable on commodity hardware, which an itinerary-first
# router cannot do at all. The CAR baseline is not represented — 03 writes that
# file without a telemetry row — so counts cover the transit passes only.

compute_by_pass <- telemetry |>
  group_by(Pass = pass) |>
  summarise(
    Scenarios          = n_distinct(scenario),
    `Origins per call` = max(n_origins),
    `Arrival instants` = n_distinct(arrival),
    Calls              = n(),
    `O-D travel times` = sum(n_origins) * n_fac,
    `Median s / call`  = median(elapsed_s),
    `Total min`        = sum(elapsed_s) / 60,
    .groups = "drop"
  )

compute_total <- tibble::tibble(
  Pass               = "All",
  Scenarios          = NA_integer_,
  `Origins per call` = NA_integer_,
  `Arrival instants` = NA_integer_,
  Calls              = sum(compute_by_pass$Calls),
  `O-D travel times` = sum(compute_by_pass$`O-D travel times`),
  `Median s / call`  = median(telemetry$elapsed_s),
  `Total min`        = sum(telemetry$elapsed_s) / 60
)

tab1a <- bind_rows(compute_by_pass, compute_total) |>
  mutate(
    across(c(Scenarios, `Origins per call`, `Arrival instants`, Calls),
           ~ ifelse(is.na(.x), "", format(.x, big.mark = ","))),
    `O-D travel times` = format(`O-D travel times`, big.mark = ","),
    `Median s / call`  = sprintf("%.0f", `Median s / call`),
    `Total min`        = sprintf("%.1f", `Total min`)
  )

throughput <- sum(telemetry$n_origins) * n_fac / sum(telemetry$elapsed_s)

kbl(tab1a, format = "html", align = c("l", rep("r", 7)),
    caption = "Table 1a. Computational cost of the routing design.") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  row_spec(nrow(tab1a), bold = TRUE) |>
  footnote(
    general = sprintf(
      paste("%s origin-destination travel times computed in %.1f minutes",
            "(%.0f per second) across %d facilities. %s, R %s, %s GB JVM heap,",
            "%d logical cores. Peak memory was not instrumented; the figure",
            "given is allocated heap. Car-mode baseline (%s origins, single",
            "departure) is excluded: it is written without a telemetry record.",
            "Analysis date %s; service window %s."),
      format(sum(telemetry$n_origins) * n_fac, big.mark = ","),
      sum(telemetry$elapsed_s) / 60,
      throughput, n_fac,
      paste0("r5r ", mf(manifest$environment$r5r)),
      mf(manifest$environment$r),
      mf(manifest$environment$java_heap_gb),
      parallel::detectCores(logical = TRUE),
      format(n_distinct(od_hour$origin_id), big.mark = ","),
      mf(manifest$analysis_date), mf(manifest$service_window)
    ),
    general_title = "", threeparttable = TRUE
  ) |>
  save_kable(file.path(out_dir, "table1a_compute.html"))

message("table 1a written")


################################################################################
# TABLE 1b  routing success, tiers and composition
################################################################################
# Two independent groupings on shared columns rather than nesting bands inside
# facilities: three of the five facilities have fewer than twenty reachable
# origins, and cross-tabulating those by band gives cells of four and five.
#
# Reachability and tiers come from od_pair (tier on the MEDIAN travel time
# across arrival hours, per Methods 2.4). Composition comes from od_hour
# reachable rows, so shares are per-trip proportions.

prim_pair <- filter(od_pair, scenario == PRIMARY) |>
  mutate(band = band_of(median_total_time))

prim_hour <- filter(od_hour, scenario == PRIMARY, reachable) |>
  mutate(band = band_of(total_time))

# access + egress collapse to walk: Discussion 4.1 argues in terms of walking,
# waiting and transferring, not five separate legs
composition <- function(df, group) {
  df |>
    group_by(across(all_of(group))) |>
    summarise(
      n_trips  = n(),
      med      = median(total_time),
      walk     = 100 * median((access_time + egress_time) / total_time),
      wait     = 100 * median(wait_time / total_time),
      ride     = 100 * median(ride_time / total_time),
      transfer = 100 * median(transfer_time / total_time),
      multi_op = 100 * mean(n_agencies > 1),
      .groups  = "drop"
    )
}

by_facility <- prim_pair |>
  group_by(site) |>
  summarise(
    n_tested  = n(),
    n_reach   = sum(!is.na(median_total_time)),
    pct_reach = 100 * n_reach / n_tested,
    med_pair  = median(median_total_time, na.rm = TRUE),
    n_b1      = sum(band == BAND_LABS[1], na.rm = TRUE),
    n_b2      = sum(band == BAND_LABS[2], na.rm = TRUE),
    n_b3      = sum(band == BAND_LABS[3], na.rm = TRUE),
    .groups   = "drop"
  ) |>
  left_join(composition(prim_hour, "site"), by = "site") |>
  arrange(desc(n_reach)) |>
  transmute(
    Row = site,
    N          = format(n_reach, big.mark = ","),
    `Reach %`  = sprintf("%.1f", pct_reach),
    Median     = sprintf("%.1f", med_pair),
    b1 = format(n_b1, big.mark = ","),
    b2 = format(n_b2, big.mark = ","),
    b3 = format(n_b3, big.mark = ","),
    Walk     = sprintf("%.0f", walk),
    Wait     = sprintf("%.0f", wait),
    `In-veh` = sprintf("%.0f", ride),
    Transfer = sprintf("%.0f", transfer),
    `Multi-op %` = sprintf("%.0f", multi_op)
  )

by_band <- composition(prim_hour, "band") |>
  arrange(band) |>
  transmute(
    Row = as.character(band),
    N          = format(n_trips, big.mark = ","),
    `Reach %`  = "",
    Median     = sprintf("%.1f", med),
    b1 = "", b2 = "", b3 = "",
    Walk     = sprintf("%.0f", walk),
    Wait     = sprintf("%.0f", wait),
    `In-veh` = sprintf("%.0f", ride),
    Transfer = sprintf("%.0f", transfer),
    `Multi-op %` = sprintf("%.0f", multi_op)
  )

tab1b <- bind_rows(by_facility, by_band)
names(tab1b)[names(tab1b) == "Row"] <- " "
names(tab1b)[names(tab1b) == "b1"]  <- BAND_LABS[1]
names(tab1b)[names(tab1b) == "b2"]  <- BAND_LABS[2]
names(tab1b)[names(tab1b) == "b3"]  <- BAND_LABS[3]

kbl(tab1b, format = "html", align = c("l", rep("r", 11)),
    caption = "Table 1b. Routing success, travel-time tiers and journey composition, primary specification.") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  add_header_above(c(" " = 4, "Origins by median travel time" = 3,
                     "Share of travel time (%)" = 4, " " = 1)) |>
  pack_rows(index = c("By facility" = nrow(by_facility),
                      "By travel-time band, all facilities pooled" = nrow(by_band))) |>
  footnote(
    general = sprintf(
      paste("N is reachable origins in the facility rows and reachable trips in",
            "the band rows. Reachable = an itinerary within %d minutes at one or",
            "more of the eight arrival hours, walking at %.1f km/h with at most",
            "%d minutes per walk leg and %d rides; the >60 band is therefore",
            "censored at %d. Tiers are assigned on each origin's median travel",
            "time across arrival hours. Walk combines access and egress. Shares",
            "are medians of per-trip proportions and need not sum to 100.",
            "Multi-op %% is the share of trips using more than one operator."),
      cfg$routing$max_trip_duration,
      cfg$scenarios$walk_speed[cfg$scenarios$is_primary],
      cfg$scenarios$max_walk_time[cfg$scenarios$is_primary],
      cfg$routing$max_rides,
      cfg$routing$max_trip_duration
    ),
    general_title = "", threeparttable = TRUE
  ) |>
  save_kable(file.path(out_dir, "table1b_access_composition.html"))

message("table 1b written")


################################################################################
# FIGURE  per-origin temporal range, CHOP_PHL
################################################################################
# Full-day p10-p90 per origin, with the within-hour p10-p90 computed inside
# each hour and averaged, so the two components are not conflated. Same
# percentile pair for both bands — comparing a within-hour IQR against a
# full-day p10-p90 would make the narrower pair look tighter regardless of
# where the variance sits.

per_hour <- od_instant |>
  filter(site == FOCUS_SITE, reachable) |>
  group_by(origin_id, arrival_hour) |>
  summarise(hr_p10 = quantile(total_time, 0.10),
            hr_p90 = quantile(total_time, 0.90),
            .groups = "drop")

org <- od_instant |>
  filter(site == FOCUS_SITE, reachable) |>
  group_by(origin_id) |>
  summarise(n_reach = n(),
            p10 = quantile(total_time, 0.10),
            p50 = median(total_time),
            p90 = quantile(total_time, 0.90),
            .groups = "drop") |>
  left_join(per_hour |>
              group_by(origin_id) |>
              summarise(within_lo = mean(hr_p10), within_hi = mean(hr_p90),
                        .groups = "drop"),
            by = "origin_id") |>
  filter(n_reach >= MIN_REACH) |>
  mutate(span         = p90 - p10,
         within_span  = within_hi - within_lo,
         within_share = within_span / span) |>
  arrange(p50) |>
  mutate(rank = row_number())

# manuscript numbers for Results 3.2
cross_mat <- sapply(BANDS, function(t) org$p10 < t & org$p90 > t)
message(sprintf("origin range, %s: %d origins with >= %d reachable instants",
                FOCUS_SITE, nrow(org), MIN_REACH))
message(sprintf("  cross %d min: %d (%.1f%%)   cross %d min: %d (%.1f%%)   any: %d (%.1f%%)",
                BANDS[1], sum(cross_mat[, 1]), 100 * mean(cross_mat[, 1]),
                BANDS[2], sum(cross_mat[, 2]), 100 * mean(cross_mat[, 2]),
                sum(rowSums(cross_mat) > 0), 100 * mean(rowSums(cross_mat) > 0)))
message(sprintf("  median within-hour share of total spread: %.0f%%",
                100 * median(org$within_share, na.rm = TRUE)))

fig_range <- ggplot(org, aes(y = rank)) +
  geom_linerange(aes(xmin = p10, xmax = p90),
                 colour = pal$stable, alpha = 0.55, linewidth = 0.35) +
  geom_linerange(aes(xmin = within_lo, xmax = within_hi),
                 colour = pal$signal, linewidth = 0.35) +
  geom_point(aes(x = p50), colour = "grey25", size = 0.25) +
  geom_vline(xintercept = BANDS, linetype = "dashed",
             colour = "grey45", linewidth = 0.3) +
  labs(
    title = sprintf("Travel time range by origin, %s", FOCUS_SITE),
    subtitle = paste("Gold: 10th-90th percentile across the full day.",
                     "Indigo: mean of the eight within-hour 10th-90th percentile ranges.",
                     "\nPoint: median. Origins ordered by median.",
                     "Dashed lines are the 30- and 60-minute thresholds."),
    x = "Travel time (min)", y = "Origin (ranked by median)"
  ) +
  theme_plot() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

save_fig(fig_range, "figure_origin_range", w = 8, h = 6)
message("origin range figure written")


################################################################################
# MAPS  shared geometry
################################################################################
# Layer order for every map: land -> water -> origin points -> transit routes
# -> facility crosses. Routes sit OVER the points so the corridor a cluster of
# reachable origins hangs off stays visible.
#
# The window is a VIEWPORT (coord_sf), not a crop: all origins stay in the
# objects and only the drawn extent changes. Route geometry IS cropped since
# nothing counts it.

origins_raw <- read.csv(cfg$origins$file, colClasses = "character")
origins_sf <- data.frame(
  origin_id = origins_raw[[cfg$origins$id_field]],
  lon       = as.numeric(origins_raw[[cfg$origins$lon_field]]),
  lat       = as.numeric(origins_raw[[cfg$origins$lat_field]]),
  stringsAsFactors = FALSE
) |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(CRS_MAP)

facilities_sf <- cfg$facilities |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(CRS_MAP)

map_bbox <- function(x, buffer_km, square = TRUE) {
  bb <- st_bbox(st_buffer(st_union(x), buffer_km * 1000))
  if (square) {
    dx <- bb[["xmax"]] - bb[["xmin"]]
    dy <- bb[["ymax"]] - bb[["ymin"]]
    pad <- (max(dx, dy) - c(dx, dy)) / 2
    bb <- bb + c(-pad[1], -pad[2], pad[1], pad[2])
  }
  bb
}

VIEW      <- map_bbox(facilities_sf, VIEW_BUFFER_KM)
view_poly <- st_as_sfc(VIEW)

coord_view <- list(coord_sf(
  xlim = c(VIEW[["xmin"]], VIEW[["xmax"]]),
  ylim = c(VIEW[["ymin"]], VIEW[["ymax"]]),
  expand = FALSE, crs = st_crs(CRS_MAP)
))

in_view <- lengths(st_intersects(origins_sf, view_poly)) > 0
view_note <- sprintf("Window is the facility extent plus %d km; %d of %d origins (%.0f%%) lie outside it.",
                     VIEW_BUFFER_KM, sum(!in_view), nrow(origins_sf), 100 * mean(!in_view))
message(sprintf("viewport: %d of %d origins inside", sum(in_view), nrow(origins_sf)))

# --- GTFS routes ---------------------------------------------------------------

read_gtfs_table <- function(target, need, build) {
  zips <- list.files(cfg$paths$network, pattern = "\\.zip$", full.names = TRUE)
  out <- lapply(zips, function(z) {
    entries <- utils::unzip(z, list = TRUE)$Name
    hit <- entries[basename(entries) == target]
    if (length(hit) == 0) return(NULL)
    s <- read.csv(unz(z, hit[1]), colClasses = "character")
    names(s) <- gsub("^[^A-Za-z_]+", "", names(s))   # strip BOM artefacts
    if (!all(need %in% names(s))) return(NULL)
    build(s, tools::file_path_sans_ext(basename(z)))
  })
  do.call(rbind, out)
}

is_rail <- function(feed) grepl("rail|patco|amtrak", feed, ignore.case = TRUE)

build_routes <- function() {
  shapes <- read_gtfs_table(
    "shapes.txt",
    c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
    function(s, feed) data.frame(
      feed = feed, shape_id = s$shape_id,
      seq = as.integer(s$shape_pt_sequence),
      lat = as.numeric(s$shape_pt_lat), lon = as.numeric(s$shape_pt_lon),
      stringsAsFactors = FALSE))
  if (is.null(shapes) || nrow(shapes) == 0) return(NULL)

  trips <- read_gtfs_table(
    "trips.txt", c("route_id", "shape_id"),
    function(s, feed) data.frame(feed = feed, route_id = s$route_id,
                                 shape_id = s$shape_id, stringsAsFactors = FALSE))

  ln <- shapes |>
    filter(!is.na(lat), !is.na(lon), !is.na(seq)) |>
    mutate(rail = is_rail(feed)) |>
    group_by(feed, shape_id) |> filter(n() >= 2) |> ungroup() |>
    arrange(feed, shape_id, seq) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(CRS_MAP) |>
    group_by(feed, shape_id, rail) |>
    summarise(do_union = FALSE, .groups = "drop") |>
    st_cast("LINESTRING")
  ln <- suppressWarnings(st_crop(ln, VIEW))

  # one shape per route — the longest — so pattern variants do not overplot
  if (!is.null(trips)) {
    ln <- ln |>
      left_join(distinct(trips, feed, route_id, shape_id), by = c("feed", "shape_id")) |>
      mutate(len = as.numeric(st_length(geometry))) |>
      group_by(feed, route_id) |> slice_max(len, n = 1, with_ties = FALSE) |> ungroup()
  }
  ln
}

routes <- NULL
if (SHOW_ROUTES) {
  routes <- tryCatch(build_routes(), error = function(e) {
    message("routes unavailable (", conditionMessage(e), ")"); NULL })
  if (!is.null(routes)) message(sprintf("routes in view: %d (%d rail)", nrow(routes), sum(routes$rail)))
}

# --- counties (for the NHD AOI only) and water --------------------------------

counties <- if (!is.null(COUNTY_SHP)) {
  st_read(COUNTY_SHP, quiet = TRUE) |> st_transform(CRS_MAP)
} else {
  tryCatch({
    suppressMessages(tigris::counties(state = c("PA", "NJ", "DE"), cb = TRUE,
                                      progress_bar = FALSE)) |> st_transform(CRS_MAP)
  }, error = function(e) { message("counties unavailable (", conditionMessage(e), ")"); NULL })
}
if (!is.null(counties)) counties <- counties[lengths(st_intersects(counties, view_poly)) > 0, ]

area <- NULL
if (!is.null(counties)) {
  area <- tryCatch({
    aoi <- counties |> st_union() |> st_sf()
    get_nhdarea(AOI = aoi) |> st_transform(CRS_MAP) |> filter(areasqkm >= MIN_WATER_KM2)
  }, error = function(e) { message("NHD water unavailable (", conditionMessage(e), ")"); NULL })
  if (!is.null(area)) message(sprintf("NHD water polygons >= %d km2: %d", MIN_WATER_KM2, nrow(area)))
}

# origins landing in open water are geocoding artefacts; dropped once so every
# map shares a denominator
if (!is.null(area) && nrow(area) > 0) {
  n_before   <- nrow(origins_sf)
  origins_sf <- origins_sf[lengths(st_intersects(origins_sf, area)) == 0, ]
  message(sprintf("origins dropped for landing in water: %d", n_before - nrow(origins_sf)))
}

# --- layers -------------------------------------------------------------------

base_layers <- function() {
  l <- list(annotate("rect",
                     xmin = VIEW[["xmin"]], xmax = VIEW[["xmax"]],
                     ymin = VIEW[["ymin"]], ymax = VIEW[["ymax"]],
                     fill = pal$land, colour = NA))
  if (!is.null(area)) {
    l <- c(l, list(geom_sf(data = area, fill = pal$water, colour = pal$water, alpha = 0.3)))
  }
  l
}

route_layers <- function() {
  if (!SHOW_ROUTES || is.null(routes)) return(list())
  list(
    geom_sf(data = filter(routes, !rail), colour = pal$bus,  linewidth = 0.18, alpha = 0.7),
    geom_sf(data = filter(routes,  rail), colour = pal$rail, linewidth = 0.45, alpha = 0.7)
  )
}

# halo disc underneath, brick cross on top; the halo does the finding
facility_layer <- function(data = facilities_sf, size, stroke) list(
  geom_sf(data = data, shape = 21, alpha = 0.6, colour = pal$halo2, fill = pal$halo,
          size = size, stroke = stroke),
  geom_sf(data = data, shape = 3, colour = pal$facility, size = size * 0.8, stroke = stroke * 0.8)
)

overlay_layers <- function(fac = facilities_sf, size = 3, stroke = 3, routes_on = TRUE) {
  c(if (routes_on) route_layers() else list(), facility_layer(fac, size, stroke))
}


################################################################################
# MAP A  facilities reachable by transit
################################################################################

mapA_dat <- origins_sf |>
  left_join(origin_level |> filter(scenario == PRIMARY) |>
              select(origin_id, reachable_count), by = "origin_id") |>
  mutate(reachable_count = ifelse(is.na(reachable_count), 0L, reachable_count)) |>
  arrange(reachable_count)   # zero first, so reachable points sit on top

if (max(mapA_dat$reachable_count) > 2) {
  stop("reachable_count exceeds 2; extend pal$access — the gold-to-indigo ",
       "scale is defined for three levels only")
}
mapA_dat$n_fac <- factor(mapA_dat$reachable_count, levels = names(pal$access))

mapA <- ggplot() +
  base_layers() +
  geom_sf(data = mapA_dat, aes(colour = n_fac, size = ifelse(n_fac == "2", 1, 0)),
          shape = 19, alpha = 0.8) +
  overlay_layers() +
  # second pass on the two-facility origins so they sit above the routes
  geom_sf(data = filter(mapA_dat, n_fac == "2"), aes(colour = n_fac),
          size = 1.2, shape = 19, alpha = 0.4) +
  coord_view +
  scale_colour_manual(values = pal$access, name = "Facilities reachable", drop = FALSE) +
  scale_size_continuous(range = c(0.8, 1.5)) +
  labs(title = "Facilities reachable by transit",
       subtitle = sprintf("Each point is one synthetic patient origin, within %d minutes at any of the eight arrival hours.\n%s",
                          cfg$routing$max_trip_duration, view_note)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3)),
         size = "none") +
  theme_map()

save_fig(mapA, "mapA_choice_collapse", w = 6.5, h = 6.8)
message("map A written")


################################################################################
# MAP C  where the appointment hour changes the band, CHOP_PHL
################################################################################
# Single site. flip_dat must be one row per origin or the join below doubles
# the point set; a two-site version needs a facet or an "either site" collapse.

flip_dat <- od_hour |>
  filter(scenario == PRIMARY, site == FOCUS_SITE) |>
  mutate(band = band_of(total_time)) |>
  group_by(origin_id) |>
  summarise(n_bands = n_distinct(band),
            ever    = any(!is.na(total_time)), .groups = "drop") |>
  mutate(status = case_when(
    !ever       ~ "Never reachable",
    n_bands > 1 ~ "Band changes across the day",
    TRUE        ~ "Stable band"
  ))

mapC_dat <- origins_sf |>
  left_join(flip_dat, by = "origin_id") |>
  mutate(status = ifelse(is.na(status), "Never reachable", status),
         status = factor(status, c("Never reachable", "Stable band",
                                   "Band changes across the day"))) |>
  arrange(status)

message("map C status counts:")
print(table(mapC_dat$status))

mapC <- ggplot() +
  base_layers() +
  geom_sf(data = mapC_dat, aes(colour = status), size = 0.85, alpha = 0.85) +
  overlay_layers() +
  coord_view +
  scale_colour_manual(values = c("Never reachable" = pal$none,
                                 "Stable band" = pal$stable,
                                 "Band changes across the day" = pal$signal),
                      name = NULL) +
  labs(title = sprintf("Where the appointment hour changes the answer, %s", FOCUS_SITE),
       subtitle = sprintf("Origins whose travel-time band differs across the eight arrival hours.\nBands: %s. %s",
                          paste(BAND_LABS, collapse = ", "), view_note)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3))) +
  theme_map()

save_fig(mapC, "mapC_flip", w = 6.5, h = 6.8)
message("map C written")

message("\nall outputs in ", out_dir)




mapD_dat <- origins_sf |>
  left_join(od_pair, by = "origin_id") |> 
  filter(site=="CHOP_PHL", scenario=="primary")


mapD <- ggplot() +
  base_layers() +
  geom_sf(data = mapD_dat, aes(colour = as.character(median_agencies),
                               #size = ifelse(median_agencies > 0, 1, 0)
                               ),
          shape = 19, alpha = 0.8) +
  overlay_layers() +
  # second pass on the two-facility origins so they sit above the routes
  geom_sf(data = filter(mapA_dat, n_fac == "2"), aes(colour = n_fac),
          size = 1.2, shape = 19, alpha = 0.4) +
  coord_view +
  scale_colour_manual(values = pal$access, name = "Transit Agencies", drop = FALSE) +
  scale_size_continuous(range = c(0.8, 1.5)) +
  labs(title = "Number Agencies to Access Care",
       subtitle = sprintf("Each point is one synthetic patient origin, within %d minutes at any of the eight arrival hours.\n%s",
                          cfg$routing$max_trip_duration, view_note)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3)),
         size = "none") +
  theme_map()

save_fig(mapD, "mapD_transfer_count", w = 6.5, h = 6.8)
message("map A written")


"#c9c6c0" "#f0a43c" "#3b2f6b" 
