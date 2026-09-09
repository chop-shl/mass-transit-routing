################################################################################
#                               04_aggregate.R
#
# Reads the RDS files written by 03 and builds clean analysis tables. Cheap and
# re-runnable: nothing here calls r5r, so thresholds and definitions can change
# without re-routing.
#
# Population pass (all origins, hourly) -> the original script's four tables,
# with `scenario` added as a grouping column:
#
#   od_hour       origin x site x hour x scenario     (T1)
#   od_pair       origin x site x scenario            (T2)
#   origin_level  origin x scenario                   (T3)
#   site_level    site x scenario                     (T4)
#
# Temporal pass (subsample, fine arrival step) cannot join that cascade — a
# different origin set and a different time grid — so it gets its own pair:
#
#   od_instant       origin x site x instant
#   od_instant_hour  origin x site x hour, within-hour percentiles and spread
#
# Headline statistics (flip rates, discordance shares, the conventional-estimate
# contrast) are one-liners off these tables and deliberately live downstream.
################################################################################

source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(jsonlite)

tables_dir <- file.path(cfg$paths$outputs, cfg$run_id, "tables")
route_dir  <- file.path(cfg$paths$outputs, cfg$run_id, "route")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

manifest       <- read_json(file.path(cfg$paths$network, "network_manifest.json"))
analysis_date  <- manifest$analysis_date
service_window <- manifest$service_window


# HELPERS
# ------------------------------------------------------------------------------

# Agency count from r5r's pipe-separated routes string. Route ids were
# namespaced by 01 as <prefix>_<id>, so the operator is the part before the
# first underscore. Evaluated over unique strings only — running this per row
# over 400k rows is minutes of pointless work.
count_agencies <- function(routes_string) {
  if (is.na(routes_string)) return(0L)
  parts <- unlist(strsplit(routes_string, "\\|", fixed = FALSE))
  parts <- parts[grepl("_", parts)]
  length(unique(sub("_.*$", "", parts)))
}

add_n_agencies <- function(df) {
  lookup <- data.frame(routes = unique(df$routes), stringsAsFactors = FALSE)
  lookup$n_agencies <- vapply(lookup$routes, count_agencies, integer(1), USE.NAMES = FALSE)
  left_join(df, lookup, by = "routes")
}

# Burden bands, applied post-hoc. Anything not routable within
# cfg$routing$max_trip_duration is "unreachable" — a result, not missing data.
thr        <- cfg$measures$burden_thresholds
band_labs  <- c(paste0("<=", thr[1]),
                paste0(thr[-length(thr)], "-", thr[-1]),
                paste0(">", thr[length(thr)]))

burden_band <- function(t) {
  out <- as.character(cut(t, breaks = c(0, thr, Inf), labels = band_labs, right = TRUE))
  out[is.na(t)] <- "unreachable"
  factor(out, levels = c(band_labs, "unreachable"))
}

# Tier counts are built by counting the band factor and widening, so the column
# names follow cfg$measures$burden_thresholds instead of hardcoding it. The old
# form indexed band_labs[4] directly, which returns NA once there are fewer
# than four bands and silently zeroes the column rather than erroring.
tier_col <- function(x) {
  x <- gsub("<=", "le", x)
  x <- gsub(">",  "gt", x)
  gsub("[^A-Za-z0-9]+", "_", x)
}

tier_names <- function(prefix) paste0(prefix, tier_col(band_labs))

count_tiers <- function(df, group, prefix) {
  out <- df |>
    filter(!is.na(median_total_time)) |>
    mutate(tier = paste0(prefix, tier_col(as.character(median_burden)))) |>
    count(across(all_of(c(group, "tier"))), name = "n") |>
    pivot_wider(names_from = tier, values_from = n, values_fill = 0L)
  # a band with no members anywhere still needs its column
  missing <- setdiff(tier_names(prefix), names(out))
  if (length(missing) > 0) out[missing] <- 0L
  out[c(group, tier_names(prefix))]
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  dlon <- (lon2 - lon1) * pi / 180
  dlat <- (lat2 - lat1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

read_pass <- function(pattern) {
  files <- list.files(route_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("no route files matching ", pattern)
  bind_rows(lapply(files, readRDS))
}


# INPUTS
# ------------------------------------------------------------------------------
# Scenarios come from cfg, so files written under a scenario that has since been
# dropped from the design are simply not read.

scenarios   <- cfg$scenarios$scenario_label
site_ids    <- cfg$facilities$site_id
hour_labels <- sprintf("%02d:00", cfg$window$arrival_hours)

origins_raw <- read.csv(cfg$origins$file, colClasses = "character")
origins <- data.frame(
  origin_id = origins_raw[[cfg$origins$id_field]],
  lon       = as.numeric(origins_raw[[cfg$origins$lon_field]]),
  lat       = as.numeric(origins_raw[[cfg$origins$lat_field]]),
  stringsAsFactors = FALSE
) |> filter(!is.na(lat), !is.na(lon))

population <- read_pass("^population__") |>
  filter(scenario %in% scenarios) |>
  rename(origin_id = from_id, site = to_id)

message(sprintf("population: %d rows, %d scenarios, %d hours",
                nrow(population), n_distinct(population$scenario),
                n_distinct(population$arrival_hour)))


# T1  od_hour
# ------------------------------------------------------------------------------
# r5r omits unreachable pairs entirely, so the complete grid is rebuilt here and
# absent rows become unreachable. This is the original script's expand.grid
# pattern and it is what keeps "no service" countable.

od_hour <- expand.grid(
  origin_id    = origins$origin_id,
  site         = site_ids,
  arrival_hour = hour_labels,
  scenario     = scenarios,
  stringsAsFactors = FALSE
) |>
  left_join(population, by = c("origin_id", "site", "arrival_hour", "scenario")) |>
  add_n_agencies() |>
  mutate(
    reachable = !is.na(total_time),
    # walk-only trips return n_rides = 0, so the original's n_rides - 1 gave -1
    n_transfers    = pmax(n_rides - 1L, 0L),
    burden         = burden_band(total_time),
    analysis_date  = analysis_date,
    service_window = service_window
  ) |>
  select(origin_id, site, scenario, arrival_hour, departure_time,
         total_time, access_time, wait_time, ride_time, transfer_time,
         egress_time, routes, n_rides, n_transfers, n_agencies,
         reachable, burden, analysis_date, service_window)

saveRDS(od_hour, file.path(tables_dir, "od_hour.rds"))
message("T1 od_hour: ", nrow(od_hour), " rows")


# T2  od_pair
# ------------------------------------------------------------------------------

reachable_rows <- filter(od_hour, reachable)

summary_reachable <- reachable_rows |>
  group_by(origin_id, site, scenario) |>
  summarise(
    median_total_time    = median(total_time),
    median_access_time   = median(access_time),
    median_wait_time     = median(wait_time),
    median_ride_time     = median(ride_time),
    median_transfer_time = median(transfer_time),
    median_egress_time   = median(egress_time),
    median_rides         = median(n_rides),
    median_transfers     = median(n_transfers),
    median_agencies      = median(n_agencies),
    best_hour            = arrival_hour[which.min(total_time)],
    best_time            = min(total_time),
    worst_hour           = arrival_hour[which.max(total_time)],
    worst_time           = max(total_time),
    total_time_range     = max(total_time) - min(total_time),
    .groups = "drop"
  )

summary_counts <- od_hour |>
  group_by(origin_id, site, scenario) |>
  summarise(
    n_hours_reachable = sum(reachable),
    n_hours_tested    = n(),
    # does the burden band change across the appointment day? unreachable is a
    # band, so a pair that drops out at some hours counts as flipping.
    flips_between_hours = n_distinct(burden) > 1,
    .groups = "drop"
  )

od_pair <- summary_counts |>
  left_join(summary_reachable, by = c("origin_id", "site", "scenario")) |>
  mutate(
    pct_hours_reachable  = n_hours_reachable / n_hours_tested,
    time_range_pct_swing = total_time_range / median_total_time,
    median_burden        = burden_band(median_total_time),
    analysis_date        = analysis_date,
    service_window       = service_window
  )

saveRDS(od_pair, file.path(tables_dir, "od_pair.rds"))
message("T2 od_pair: ", nrow(od_pair), " rows")


# T3  origin_level
# ------------------------------------------------------------------------------

# transit-fastest site, by median travel time across the day
ranked <- od_pair |>
  filter(!is.na(median_total_time)) |>
  group_by(origin_id, scenario) |>
  # ties.method = "first" keeps ranks whole; ties break on facility order
  mutate(site_rank = rank(median_total_time, ties.method = "first")) |>
  ungroup()

fastest <- ranked |>
  filter(site_rank == 1) |>
  select(origin_id, scenario, fastest_site = site, fastest_time = median_total_time)

second <- ranked |>
  filter(site_rank == 2) |>
  select(origin_id, scenario, second_site = site, second_time = median_total_time)

# straight-line nearest
nearest_euclid <- expand.grid(
  origin_id = origins$origin_id, site = site_ids, stringsAsFactors = FALSE
) |>
  left_join(origins, by = "origin_id") |>
  left_join(cfg$facilities |>
              select(site = site_id, site_lon = lon, site_lat = lat),
            by = "site") |>
  mutate(km = haversine_km(lon, lat, site_lon, site_lat)) |>
  group_by(origin_id) |>
  slice_min(km, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(origin_id, nearest_site_euclid = site, nearest_km = km)

# drive-time nearest
car <- readRDS(file.path(route_dir, "car_baseline.rds")) |>
  rename(origin_id = from_id, site = to_id)
car_time_col <- setdiff(names(car), c("origin_id", "site"))[1]

nearest_car <- car |>
  rename(car_time = all_of(car_time_col)) |>
  filter(!is.na(car_time)) |>
  group_by(origin_id) |>
  slice_min(car_time, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(origin_id, nearest_site_car = site, nearest_car_time = car_time)

origin_access <- od_pair |>
  group_by(origin_id, scenario) |>
  summarise(
    reachable_count          = sum(!is.na(median_total_time)),
    best_pct_hours_reachable = ifelse(reachable_count > 0,
                                      max(pct_hours_reachable, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |>
  left_join(count_tiers(od_pair, c("origin_id", "scenario"), "n_sites_"),
            by = c("origin_id", "scenario")) |>
  mutate(reachable_any = reachable_count > 0)

origin_level <- expand.grid(
  origin_id = origins$origin_id, scenario = scenarios, stringsAsFactors = FALSE
) |>
  left_join(origin_access,  by = c("origin_id", "scenario")) |>
  left_join(fastest,        by = c("origin_id", "scenario")) |>
  left_join(second,         by = c("origin_id", "scenario")) |>
  left_join(nearest_euclid, by = "origin_id") |>
  left_join(nearest_car,    by = "origin_id") |>
  mutate(
    across(all_of(c("reachable_count", tier_names("n_sites_"))),
           ~ ifelse(is.na(.x), 0L, .x)),
    reachable_any = ifelse(is.na(reachable_any), FALSE, reachable_any),
    # transit-fastest is not always the closest facility: the two discordance
    # flags are what Results 3.3 is built from.
    discordant_euclid = !is.na(fastest_site) & fastest_site != nearest_site_euclid,
    discordant_car    = !is.na(fastest_site) & fastest_site != nearest_site_car,
    analysis_date     = analysis_date,
    service_window    = service_window
  )

# minutes lost by sending a patient to the straight-line nearest facility
# instead of the transit-fastest one
cost_of_nearest <- od_pair |>
  select(origin_id, scenario, site, median_total_time) |>
  inner_join(origin_level |> select(origin_id, scenario, nearest_site_euclid),
             by = c("origin_id", "scenario")) |>
  filter(site == nearest_site_euclid) |>
  select(origin_id, scenario, nearest_euclid_time = median_total_time)

origin_level <- origin_level |>
  left_join(cost_of_nearest, by = c("origin_id", "scenario")) |>
  mutate(nearest_penalty_min = nearest_euclid_time - fastest_time)

saveRDS(origin_level, file.path(tables_dir, "origin_level.rds"))
message("T3 origin_level: ", nrow(origin_level), " rows")


# T4  site_level
# ------------------------------------------------------------------------------

catchment <- origin_level |>
  filter(!is.na(fastest_site)) |>
  count(scenario, fastest_site, name = "n_origins_fastest") |>
  rename(site = fastest_site)

site_level <- od_pair |>
  group_by(site, scenario) |>
  summarise(
    n_origins_tested    = n(),
    n_origins_reachable = sum(!is.na(median_total_time)),
    median_total_time    = median(median_total_time,    na.rm = TRUE),
    median_access_time   = median(median_access_time,   na.rm = TRUE),
    median_wait_time     = median(median_wait_time,     na.rm = TRUE),
    median_ride_time     = median(median_ride_time,     na.rm = TRUE),
    median_transfer_time = median(median_transfer_time, na.rm = TRUE),
    median_egress_time   = median(median_egress_time,   na.rm = TRUE),
    median_transfers     = median(median_transfers,     na.rm = TRUE),
    median_agencies      = median(median_agencies,      na.rm = TRUE),
    median_pct_hours_reachable =
      median(pct_hours_reachable[!is.na(median_total_time)]),
    pct_flipping = mean(flips_between_hours),
    .groups = "drop"
  ) |>
  mutate(pct_origins_reachable = n_origins_reachable / n_origins_tested) |>
  # tier counts joined rather than summarised, so the columns follow the
  # thresholds; the original omitted the top tier from the site table entirely
  left_join(count_tiers(od_pair, c("site", "scenario"), "n_"),
            by = c("site", "scenario")) |>
  mutate(across(all_of(tier_names("n_")), ~ ifelse(is.na(.x), 0L, .x))) |>
  left_join(catchment, by = c("site", "scenario")) |>
  mutate(
    n_origins_fastest = ifelse(is.na(n_origins_fastest), 0L, n_origins_fastest),
    analysis_date     = analysis_date,
    service_window    = service_window
  )

saveRDS(site_level, file.path(tables_dir, "site_level.rds"))
write.csv(site_level, file.path(tables_dir, "site_level.csv"), row.names = FALSE)
message("T4 site_level: ", nrow(site_level), " rows")


# TEMPORAL PASS
# ------------------------------------------------------------------------------
# Separate origin set and time grid, so this does not join the cascade above.

temporal <- read_pass("^temporal__") |>
  rename(origin_id = from_id, site = to_id)

temporal_origins  <- sort(unique(temporal$origin_id))
temporal_instants <- sort(unique(temporal$arrival_time))

message(sprintf("temporal: %d rows, %d origins, %d instants",
                nrow(temporal), length(temporal_origins), length(temporal_instants)))

od_instant <- expand.grid(
  origin_id    = temporal_origins,
  site         = site_ids,
  arrival_time = temporal_instants,
  stringsAsFactors = FALSE
) |>
  left_join(temporal |>
              select(origin_id, site, arrival_time, arrival_hour, total_time),
            by = c("origin_id", "site", "arrival_time")) |>
  mutate(
    arrival_hour = substr(arrival_time, 1, 2),
    arrival_hour = paste0(arrival_hour, ":00"),
    reachable    = !is.na(total_time),
    burden       = burden_band(total_time)
  )

saveRDS(od_instant, file.path(tables_dir, "od_instant.rds"))
message("temporal T1 od_instant: ", nrow(od_instant), " rows")

# Within-hour distribution. Between-hour spread is one line off this table:
#   group_by(origin_id, site) |> summarise(sd(median_total_time))
pctl <- cfg$measures$percentiles

od_instant_hour <- od_instant |>
  group_by(origin_id, site, arrival_hour) |>
  summarise(
    n_instants        = n(),
    n_reachable       = sum(reachable),
    pct_reachable     = n_reachable / n_instants,
    p10_total_time    = quantile(total_time, pctl[1] / 100, na.rm = TRUE),
    p25_total_time    = quantile(total_time, pctl[2] / 100, na.rm = TRUE),
    median_total_time = quantile(total_time, pctl[3] / 100, na.rm = TRUE),
    p75_total_time    = quantile(total_time, pctl[4] / 100, na.rm = TRUE),
    p90_total_time    = quantile(total_time, pctl[5] / 100, na.rm = TRUE),
    within_hour_sd    = sd(total_time, na.rm = TRUE),
    within_hour_range = suppressWarnings(
      max(total_time, na.rm = TRUE) - min(total_time, na.rm = TRUE)),
    flips_within_hour = n_distinct(burden) > 1,
    .groups = "drop"
  ) |>
  mutate(
    within_hour_range = ifelse(is.infinite(within_hour_range), NA_real_, within_hour_range),
    analysis_date     = analysis_date,
    service_window    = service_window
  )

saveRDS(od_instant_hour, file.path(tables_dir, "od_instant_hour.rds"))
message("temporal T2 od_instant_hour: ", nrow(od_instant_hour), " rows")

message("\ntables written to ", tables_dir)
