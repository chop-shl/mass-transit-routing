################################################################################
#                                 00_config.R
#
# Parameters for the CHOP transit accessibility analysis.
#
# Declarations only. No library() calls, no I/O, no derived objects. Safe to
# source from anywhere, any number of times.
#
# MUST be sourced before any script loads r5r: options(java.parameters = ...)
# has no effect once the JVM has started, and the failure is silent — r5r gets
# a default heap and dies partway into a long run.
#
# Only dependency: `here`.
################################################################################


# JVM heap. Set before r5r loads.
JAVA_HEAP <- "-Xmx20G"
options(java.parameters = JAVA_HEAP)

# GLOBAL PARAMS
#-------------------------------------------------------------------------------

# This can make sense for a huge area c(xmin, ymin, xmax, ymax)
CLIP_PBF <- c(xmin = -76.49, ymin = 39.12, xmax = -74.02, ymax = 40.99)
                      

DATE <- as.Date("2026-08-20")  # Stick to this format - no guardrails 
REFRESH_GTFS <- FALSE # Change to TRUE if you want to update the feeds
ARRIVALS <- 9:16      # The arrival hours to run through r5r

## r5r PARAMS
#------------------------------------------------------------------------------

MODE <- c("WALK", "TRANSIT")
EGRESS <- "WALK"
MAX_TRIP_DUR <- 90L
MAX_RIDES <- 3L
MIN_STEP <- 2L
BREAK_SCOPE <- "hour"   # "hour" or "minute"; minute is hella slow
DRAWS_perMIN <- 5L
THREADS <- 6            # Using a whimpy CPU
BATCH_SIZE <- 500L
RESUME <- TRUE

#-------------------------------------------------------------------------------

cfg <- list()

# PATHS
# ------------------------------------------------------------------------------
# here::here() anchors to the .Rproj / .git root so scripts run from any wd.
# Be careful here - it could wreak havoc on your file structure

cfg$paths <- local({
  root <- here::here()
  list(
    root        = root,
    osm_sources = file.path(root, "data", "osm"),      # raw .pbf extracts (read-only)
    gtfs_raw    = file.path(root, "data", "gtfs_raw"), # as-downloaded feeds (read-only)
    network     = file.path(root, "data", "network"),  # merged .pbf + prefixed feeds + network.dat
    outputs     = file.path(root, "outputs")
  )
})

# Stable across sessions — the routing loop resumes by checking for existing
# chunk files, so a timestamp here would make every restart a cold start.
cfg$run_id <- "run_02"


# Patient ORIGINS
# ------------------------------------------------------------------------------
# Put the patient data in an origins folder

cfg$origins <- list(
  file      = file.path(cfg$paths$root, "data", "origins", "origins.csv"),
  id_field  = "point_id",
  lat_field = "lat",
  lon_field = "lon"
)


# FACILITIES
# ------------------------------------------------------------------------------
# Change as necessary - using tribble for legibility

cfg$facilities <- tibble::tribble(
  ~site_id,           ~site_name,              ~lon,        ~lat,
  "CHOP_PHL",         "CHOP Philadelphia",     -75.193771,  39.948230,
  "CHOP_KOPH",        "CHOP King of Prussia",  -75.408033,  40.087951,
  "BucksCounty_ASC",  "Bucks County ASC",      -75.223773,  40.268991,
  "BrandyWine_ASC",   "Brandywine ASC",        -75.526230,  39.886848,
  "Voorhees_ASC",     "Voorhees ASC",          -74.976538,  39.845454
)

# STREET NETWORK
# ------------------------------------------------------------------------------
# Point to the osm PBFs; it'll check for a merged file ("merged.osm.pbf) and 
# make one if it isn't found.
# YOU NEED TO HAVE OSMIUM installed to your environment path if you want to merge

cfg$osm <- list(
  merged_file   = "merged.osm.pbf",
  osmium_bin    = "osmium",
  rebuild       = F,   # TRUE forces re-merge even if merged_file exists
  clip_bbox     = CLIP_PBF,    
  clip_strategy = "complete_ways"
)


# GTFS FEEDS
# ------------------------------------------------------------------------------
# To skip re-downloading, set refresh = FALSE. Do not comment out the feeds.
# 01 names local files from the vector names below, not basename(url), because
# Amtrak ships a generic "GTFS.zip" that would collide.

cfg$gtfs <- list(
  refresh = REFRESH_GTFS,
  feeds = c(
    patco    = "https://rapid.nationalrtap.org/GTFSFileManagement/UserUploadFiles/13562/PATCO_GTFS.zip",
    amtrak   = "https://content.amtrak.com/content/gtfs/GTFS.zip",
    septa    = "https://www3.septa.org/developer/gtfs_public.zip",
    njt_rail = "https://www.njtransit.com/rail_data.zip",
    njt_bus  = "https://www.njtransit.com/bus_data.zip"
  )
)


# ANALYSIS WINDOW
# ------------------------------------------------------------------------------

cfg$window <- list(
  # Set explicitly. 01 checks it falls inside every feed's service range and
  # reports the share of services suspended that day, but does not choose it.
  analysis_date = DATE,
  timezone      = "America/New_York",
  arrival_hours = ARRIVALS   # eight appointment times, 09:00-16:00
)


# ROUTING
# ------------------------------------------------------------------------------
# arrival_travel_time_matrix() has no time_window and no percentiles argument.
# The within-hour window is built by calling it once per arrival minute:
# 8 hours x 60 = 480 arrival instants per scenario. Percentiles are computed
# downstream from those 60 rows.
#
# max_trip_duration is deliberately loose. It only prunes the search, so
# tighter cutoffs are applied in 04 rather than re-run here — which is also
# what keeps "90-minute trip" distinct from "no transit at all".

cfg$routing <- list(
  mode                = MODE,
  mode_egress         = EGRESS,
  max_trip_duration   = MAX_TRIP_DUR,
  max_rides           = MAX_RIDES,
  arrival_minute_step = MIN_STEP,
  # breakdown = TRUE is much slower. Journey composition is reported by hour,
  # not by minute, so decomposition runs only at the top of each hour.
  breakdown_scope     = "hour",   # "hour" or "minute"
  # Only matters if a feed ships frequencies.txt. r5r exposes no router seed,
  # so if one does, the run is not bit-reproducible.
  draws_per_minute    = DRAWS_perMIN,
  n_threads           = THREADS,
  verbose             = TRUE,
  progress            = TRUE,
  batch_size          = BATCH_SIZE,
  resume              = RESUME
)


# SCENARIOS
# ------------------------------------------------------------------------------
# max_walk_time is PER LEG in r5r (access, each transfer, egress), not a
# per-trip budget.

cfg$scenarios <- data.frame(
  scenario_label = c("primary", "slow_walk"),
  walk_speed     = c(3.6,       3.0),
  max_walk_time  = c(15L,       15L),
  is_primary     = c(TRUE,      FALSE),
  stringsAsFactors = FALSE
)


# NEAREST-FACILITY BASELINE
# ------------------------------------------------------------------------------
# Contrasts the transit-fastest facility against the nearest one. Euclidean is
# cheap; car requires a second r5r pass.

cfg$nearest <- list(
  methods            = c("euclidean", "car"),
  primary            = "euclidean",
  car_departure_hour = 9L,
  car_max_duration   = 180L
)


# MEASURES
# ------------------------------------------------------------------------------

cfg$measures <- list(
  burden_thresholds  = c(30L, 60L),
  burden_primary     = 60L,
  burden_sensitivity = 45L,
  percentiles        = c(10L, 25L, 50L, 75L, 90L),
  summary_percentile = 50L
)


# VALIDATION
# ------------------------------------------------------------------------------

stopifnot(
  !anyDuplicated(cfg$facilities$site_id),
  !anyDuplicated(cfg$scenarios$scenario_label),
  sum(cfg$scenarios$is_primary) == 1L,
  all(cfg$scenarios$walk_speed > 0),
  all(cfg$scenarios$max_walk_time > 0),
  inherits(cfg$window$analysis_date, "Date"),
  !is.na(cfg$window$analysis_date),
  all(cfg$window$arrival_hours %in% 0:23),
  cfg$window$timezone %in% OlsonNames(),
  60L %% cfg$routing$arrival_minute_step == 0L,
  cfg$routing$breakdown_scope %in% c("hour", "minute"),
  cfg$routing$max_trip_duration >= max(cfg$measures$burden_thresholds),
  cfg$measures$burden_primary %in% cfg$measures$burden_thresholds,
  cfg$measures$summary_percentile %in% cfg$measures$percentiles,
  cfg$nearest$primary %in% cfg$nearest$methods
)

invisible(cfg)
