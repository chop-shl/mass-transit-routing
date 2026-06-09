# =============================================================================
# 00_settings.R
#
# All the settings for the whole project live here, in one place.
# Change things HERE instead of hunting through the analysis scripts.
#
# Every other script starts by running this file with:
#   source("scripts/00_settings.R")
# =============================================================================

# --- Memory for r5r (it runs on Java) ----------------------------------------
# Set this BEFORE r5r loads. Roughly 70% of your computer's RAM.
# "20G" means 20 gigabytes. Lower it if your machine has less memory.
options(java.parameters = "-Xmx20G")

# --- Point this R session at Java 21 -----------------------------------------
# rJavaEnv installs Java once on your machine, but each new R session has to be
# told where it is BEFORE r5r/rJava loads. This line does that automatically.
# If you ever see "JAVA_HOME cannot be determined", it means this didn't run
# (or Java 21 isn't installed -- see the README setup steps).
if (requireNamespace("rJavaEnv", quietly = TRUE)) {
  try(rJavaEnv::use_java(version = 21), silent = TRUE)
}

# --- mapview / tigris options -------------------------------------------------
# fgb = FALSE matches your original (avoids a rendering quirk in RStudio).
# tigris caching avoids re-downloading the same tract shapes every session.
suppressPackageStartupMessages({
  library(mapview)
  library(tigris)
})
mapviewOptions(fgb = FALSE)
options(tigris_use_cache = TRUE)

# --- Folders ------------------------------------------------------------------
# Relative to the project folder (where the .Rproj file is).
network_dir   <- "data/network"                     # OSM .pbf + GTFS zips; r5r reads here
patients_file <- "data/patients/sim_pts_v1.rds"     # your patient file (PHI)

# --- Cache files (slow steps save here so you don't recompute) ----------------
ttm_cache    <- "data/ttm_all_hours.rds"            # travel time matrix
routes_cache <- "data/detailed_routes.rds"          # detailed route geometries
batch_dir    <- "data/route_batches"                # per-batch route files (resume on crash)

# --- Hospitals / care sites ---------------------------------------------------
# Edit this table to change destinations. Keep id, lon, lat columns.
hospitals <- data.frame(
  id  = c("CHOP_Main", "CHOP_KOP", "BucksCounty_ASC",
          "BrandyWine_ASC", "Voorhees_ASC"),
  lon = c(-75.19377064882546, -75.40803285265305, -75.22377300507655,
          -75.52623023023766, -74.97653800693676),
  lat = c(39.94822977365352, 40.087951279294394, 40.26899110711235,
          39.88684806204198, 39.84545427341185)
)

# --- Service area buffer ------------------------------------------------------
# Patients within this many meters of a transit line are kept; others dropped.
service_area_buffer_m <- 1000   # 1 km

# --- Routing settings (passed to r5r) -----------------------------------------
# Times are in MINUTES. walk_speed is in km/h.
# NOTE: mode = "TRANSIT" matches your original. With TRANSIT, r5r still allows
# walking to/from stops, but the trip must use transit -- it won't return a
# pure walking route. This is usually what you want for a transit study.
routing_mode      <- "TRANSIT"
max_trip_duration <- 180   # ignore trips longer than this (minutes)
max_walk_time     <- 30    # most someone will walk to/from a stop (minutes)
walk_speed        <- 3.3   # km/h
max_rides         <- 4     # max number of transfers
time_window       <- 45    # spread of departures used for the percentile (minutes)
travel_percentile <- 25    # 25 = a "good" (fast) trip; 50 = typical

# Departure times for the travel-time matrix (captures variation across hours).
# IMPORTANT: r5r only routes on dates inside the GTFS calendar. If you get a
# "less than X% of transit services are running" warning, run check_gtfs_dates()
# (in 04_detailed_routes.R) and change the dates below to a covered date.
departure_times <- seq(
  from = as.POSIXct("2026-05-29 06:00:00"),
  to   = as.POSIXct("2026-05-29 08:00:00"),
  by   = "1 hour"
)

# A single departure used for drawing the detailed route lines on a map.
route_departure <- as.POSIXct("2026-05-29 08:00:00")

# How many origins to process per batch (keeps Java memory stable).
ttm_batch_size   <- 500   # for the travel time matrix
route_batch_size <- 200   # for the detailed itineraries

# --- Census (ACS) counties to pull --------------------------------------------
# FIPS codes. PA = 42, NJ = 34, DE = 10.
pa_counties <- c("101", "091", "045", "029", "017")  # Phila, Montco, Delco, Chester, Bucks
nj_counties <- c("007", "005", "015", "033", "021", "001", "029", "011", "025")
de_counties <- c("003")                               # New Castle
acs_year    <- 2022