################################################################################
#                                 03_route.R
#
# Runs r5r and writes one RDS per call. Nothing accumulates in memory.
#
# Two passes, because cost is linear in origins and the budget is fixed:
#
#   population  all origins, top of each hour, breakdown = TRUE
#               -> discordance, journey composition, burden classification
#   temporal    subsample of origins, fine arrival step, breakdown = FALSE
#               -> within-hour and between-hour spread, flip rate
#
# Jobs run in priority order and the script stops starting new ones once
# MAX_HOURS is reached, so an overrun drops repo-only sensitivity scenarios
# rather than manuscript results. Completed jobs are skipped on re-run.
################################################################################

source(here::here("R", "00_config.R"))

library(r5r)
rJava::.jinit()   # rJava starts the JVM lazily; force it before checking heap

MAX_HOURS     <- Inf
TEMPORAL_N    <- 1000L   # origins in the temporal pass; Methods 2.1 decision (set to 1000)
TEMPORAL_SCEN <- "primary"

route_dir <- file.path(cfg$paths$outputs, cfg$run_id, "route")
dir.create(route_dir, recursive = TRUE, showWarnings = FALSE)

heap_gb <- rJava::.jcall(
  rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime"),
  "J", "maxMemory") / 1024^3
message(sprintf("jvm heap: %.1f GB", heap_gb))

net <- build_network(cfg$paths$network, overwrite = FALSE)


# POINTS
# ------------------------------------------------------------------------------
# colClasses = "character" so ids keep leading zeros and never go scientific.

origins_raw <- read.csv(cfg$origins$file, colClasses = "character")

origins <- data.frame(
  id  = origins_raw[[cfg$origins$id_field]],
  lon = as.numeric(origins_raw[[cfg$origins$lon_field]]),
  lat = as.numeric(origins_raw[[cfg$origins$lat_field]]),
  stringsAsFactors = FALSE
)
origins <- origins[!is.na(origins$lat) & !is.na(origins$lon), ]
stopifnot(!anyDuplicated(origins$id), nrow(origins) > 0)

# Deterministic subsample for the temporal pass. If 02 supplies a stratified
# draw, read that instead — this is a plain seeded sample.
set.seed(1)
temporal_ids     <- sample(origins$id, min(TEMPORAL_N, nrow(origins)))
origins_temporal <- origins[origins$id %in% temporal_ids, ]

facilities <- data.frame(
  id  = cfg$facilities$site_id,
  lon = cfg$facilities$lon,
  lat = cfg$facilities$lat,
  stringsAsFactors = FALSE
)

message(sprintf("origins: %d population, %d temporal; facilities: %d",
                nrow(origins), nrow(origins_temporal), nrow(facilities)))


# WORK QUEUE
# ------------------------------------------------------------------------------

scen          <- as.data.frame(cfg$scenarios)
primary_label <- scen$scenario_label[scen$is_primary]

arrival_at <- function(hour, minute = 0L) {
  as.POSIXct(sprintf("%s %02d:%02d:00", format(cfg$window$analysis_date), hour, minute),
             tz = cfg$window$timezone)
}

# population: every scenario, top of each hour
pop_jobs <- do.call(rbind, lapply(seq_len(nrow(scen)), function(i) {
  data.frame(
    pass      = "population",
    scenario  = scen$scenario_label[i],
    hour      = cfg$window$arrival_hours,
    minute    = 0L,
    breakdown = TRUE,
    # manuscript scenarios first, repo-only sensitivity last
    priority  = if (scen$scenario_label[i] == primary_label) 1L
                else if (scen$scenario_label[i] == "long_walk") 2L else 4L,
    stringsAsFactors = FALSE
  )
}))

# temporal: one scenario, fine arrival step
temporal_jobs <- expand.grid(
  hour   = cfg$window$arrival_hours,
  minute = seq(0L, 59L, by = MIN_STEP),
  stringsAsFactors = FALSE
)
temporal_jobs$pass      <- "temporal"
temporal_jobs$scenario  <- TEMPORAL_SCEN
temporal_jobs$breakdown <- FALSE
temporal_jobs$priority  <- 3L

jobs <- rbind(pop_jobs, temporal_jobs[names(pop_jobs)])
jobs <- merge(jobs, scen[c("scenario_label", "walk_speed", "max_walk_time")],
              by.x = "scenario", by.y = "scenario_label", all.x = TRUE)
jobs$job_id <- sprintf("%s__%s__%02d%02d", jobs$pass, jobs$scenario, jobs$hour, jobs$minute)
jobs$file   <- file.path(route_dir, paste0(jobs$job_id, ".rds"))
jobs <- jobs[order(jobs$priority, jobs$scenario, jobs$hour, jobs$minute), ]

message(sprintf("queued %d jobs (%d population, %d temporal)",
                nrow(jobs), sum(jobs$pass == "population"), sum(jobs$pass == "temporal")))


# CAR BASELINE
# ------------------------------------------------------------------------------
# Drive time to each facility, for the nearest-vs-fastest contrast. One call,
# departure-based: congestion is not modelled so a window would add nothing.

car_file <- file.path(route_dir, "car_baseline.rds")
if (!file.exists(car_file) || !cfg$routing$resume) {
  t0 <- Sys.time()
  car <- travel_time_matrix(
    net,
    origins            = origins,
    destinations       = facilities,
    mode               = "CAR",
    departure_datetime = arrival_at(cfg$nearest$car_departure_hour),
    max_trip_duration  = cfg$nearest$car_max_duration,
    n_threads          = cfg$routing$n_threads,
    progress           = FALSE
  )
  saveRDS(car, car_file)
  message(sprintf("car baseline: %d rows in %.1f s",
                  nrow(car), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
} else {
  message("car baseline: cached")
}


# ROUTING LOOP
# ------------------------------------------------------------------------------

telemetry_file <- file.path(cfg$paths$outputs, cfg$run_id, "run_telemetry.rds")
telemetry <- if (file.exists(telemetry_file)) readRDS(telemetry_file) else NULL

started <- Sys.time()
n_done  <- 0L
n_skip  <- 0L

for (i in seq_len(nrow(jobs))) {

  job <- jobs[i, ]

  if (cfg$routing$resume && file.exists(job$file)) {
    n_skip <- n_skip + 1L
    next
  }

  elapsed_h <- as.numeric(difftime(Sys.time(), started, units = "hours"))
  if (elapsed_h > MAX_HOURS) {
    message(sprintf("\nstopping: %.1f h elapsed, %d jobs left unrun",
                    elapsed_h, nrow(jobs) - i + 1L))
    break
  }

  o <- if (job$pass == "temporal") origins_temporal else origins

  t0  <- Sys.time()
  res <- arrival_travel_time_matrix(
    net,
    origins           = o,
    destinations      = facilities,
    mode              = cfg$routing$mode,
    mode_egress       = cfg$routing$mode_egress,
    arrival_datetime  = arrival_at(job$hour, job$minute),
    max_trip_duration = cfg$routing$max_trip_duration,
    max_walk_time     = job$max_walk_time,
    walk_speed        = job$walk_speed,
    max_rides         = cfg$routing$max_rides,
    breakdown         = job$breakdown,
    n_threads         = cfg$routing$n_threads,
    progress          = FALSE
  )
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # r5r omits unreachable pairs entirely, so these files are sparse. 04 rebuilds
  # the complete origin x facility x instant grid; do not infer reachability
  # from row counts here.
  res$pass         <- job$pass
  res$scenario     <- job$scenario
  res$arrival_hour <- sprintf("%02d:00", job$hour)
  res$arrival_time <- sprintf("%02d:%02d", job$hour, job$minute)
  saveRDS(res, job$file)

  telemetry <- rbind(telemetry, data.frame(
    job_id    = job$job_id,
    pass      = job$pass,
    scenario  = job$scenario,
    arrival   = sprintf("%02d:%02d", job$hour, job$minute),
    n_origins = nrow(o),
    breakdown = job$breakdown,
    n_rows    = nrow(res),
    elapsed_s = round(secs, 1),
    run_at    = format(t0, tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  ))
  saveRDS(telemetry, telemetry_file)

  n_done <- n_done + 1L

  # Projection from measured rate, so an infeasible run is obvious in minutes
  # rather than at hour 20.
  rate <- as.numeric(difftime(Sys.time(), started, units = "secs")) / n_done
  left <- sum(!file.exists(jobs$file))
  message(sprintf("[%3d/%3d] %-38s %6.1f s  %6d rows   eta %.1f h",
                  i, nrow(jobs), job$job_id, secs, nrow(res), left * rate / 3600))
}

message(sprintf("\ndone: %d run, %d skipped, %.2f h elapsed",
                n_done, n_skip, as.numeric(difftime(Sys.time(), started, units = "hours"))))
message("route files in ", route_dir)
