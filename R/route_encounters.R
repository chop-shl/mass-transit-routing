################################################################################
#                            05_route_encounters.R
#
# Operational tool. Point it at a patient-encounter extract, name the columns,
# and it returns the same table with transit travel time and its components
# appended.
#
#     Rscript R/05_route_encounters.R
#
# Everything is configured in the CONFIG block below. Everything else comes from
# 00_config.R, and the network is the one 01_network.R already built — this
# script never downloads a feed, never merges a .pbf and never rebuilds.
#
# Unlike 03_route.R this is NOT the research pass. 03 routes a fixed synthetic
# origin set across a grid of hours and scenarios. Here every row carries its
# own appointment time and its own facility, the origins are whatever the
# extract happens to contain, and there is one output row per encounter.
#
# ------------------------------------------------------------------------------
# PHI
#   Routing happens in-process against a network on this machine. No coordinate
#   leaves the box. The OUTPUT still contains patient coordinates, so OUT_PATH
#   should point somewhere outside the repo and outside any sync folder. The
#   default under cfg$paths$outputs is inside the repo — change it.
#
# WHAT THE TIMES MEAN
#   Encounter timestamps are treated as ARRIVAL deadlines, not departures, which
#   is what arrival_travel_time_matrix() answers: the latest departure that still
#   lands before the appointment. That is what a patient actually does, and it is
#   the same call 03 makes.
#
#   Times are SCHEDULED service. No bunching, no breakdowns, no crowding.
#
# DATES
#   The network is built from feeds covering one service window (01 validates
#   this and records it in network_manifest.json). An encounter extract spans a
#   fiscal year, so most encounter dates have no feed behind them. Each encounter
#   is therefore mapped onto a representative date of the same weekday inside
#   that window, preserving time of day.
#
#   So a result means "a typical Tuesday at 14:20", not "that specific Tuesday".
#   Same caveat the drive-time script carries about Valhalla's weekly traffic
#   profile, for the same reason, and it also collapses the calendar to seven
#   days, which is what keeps the number of r5r calls bounded.
################################################################################

source(here::here("R", "00_config.R"))

suppressPackageStartupMessages({
  library(r5r); library(dplyr); library(readr); library(jsonlite); library(sf)
})
rJava::.jinit()   # rJava starts the JVM lazily; force it before checking heap

`%||%` <- function(a, b) if (is.null(a)) b else a


# CONFIG
# ==============================================================================

IN_PATH  <- file.path(cfg$paths$root, "data", "encounters", "fy26_2.csv")
OUT_PATH <- file.path(cfg$paths$outputs, cfg$run_id, "encounters_routed.csv")

# --- column names in YOUR extract ---------------------------------------------
COL_LAT      <- "geocode_latitude"
COL_LON      <- "geocode_longitude"
COL_DATETIME <- "enc_start_datetime"
COL_FACILITY <- "FACILITY_ADDRESS"

# Carried through as a join key. NULL falls back to row order.
COL_ID <- "encounter_id"

# strptime format for COL_DATETIME. NULL auto-detects from DATETIME_FORMATS
# below, which is usually what you want: set it explicitly only to force one.
DATETIME_FORMAT <- NULL

# Tried in order; the one parsing the most rows wins. Seconds-bearing variants
# come first because strptime ignores trailing characters, so "%Y-%m-%d %H:%M"
# would happily match a string carrying seconds and silently drop them.
#
# Deliberately no day-first (%d/%m) format. On US data it would misread 03/04
# as 4 March rather than 3 April on exactly the rows where it matters and never
# fail loudly. Add one here if your extract is genuinely day-first.
DATETIME_FORMATS <- c(
  "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M",
  "%m/%d/%Y %H:%M:%S", "%m/%d/%y %H:%M:%S",
  "%m/%d/%Y %H:%M",    "%m/%d/%y %H:%M",
  "%m/%d/%Y %I:%M %p", "%m/%d/%y %I:%M %p",
  "%d-%b-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S", "%d-%b-%y %H:%M")

# Collapse repeated rows for the same encounter. Extracts carry one row per
# order, charge or department within a visit; routing each is wasted work and
# inflates every downstream count. Rows whose duplicates DISAGREE on the routing
# inputs are reported rather than silently collapsed.
DEDUPE_ENCOUNTERS <- TRUE

# --- facility matching --------------------------------------------------------
# COL_FACILITY is free text: "3401 Civic Center Blvd,Children's Hosp of
# Phila,4 Fl East" carries the department and floor inside the address, so match
# on the fragment that identifies the site. Patterns are matched
# case-insensitively and MUST be mutually exclusive — a value matching two is
# ambiguous and picking the first would silently assign the wrong coordinates.
#
# site_id must exist in cfg$facilities; checked below.
FACILITY_PATTERNS <- tibble::tribble(
  ~pattern,            ~site_id,
  "CIVIC\\s*CENTER",   "CHOP_PHL",
  "GODDARD|KING\\s*OF\\s*PRUSSIA", "CHOP_KOPH",
  "BUCKS",             "BucksCounty_ASC",
  "BRANDYWINE",        "BrandyWine_ASC",
  "VOORHEES",          "Voorhees_ASC"
)

# Stop when a facility value matches no pattern or more than one. FALSE reports
# and carries those rows through as no_facility_match.
STRICT_FACILITY_MATCH <- FALSE

# --- routing ------------------------------------------------------------------
# Scenario from cfg$scenarios; supplies walk_speed and max_walk_time.
SCENARIO <- cfg$scenarios$scenario_label[cfg$scenarios$is_primary]

# Journey composition (access / wait / ride / transfer / egress, n_rides).
# Much slower — this is why 03 only decomposes at the top of each hour. Here
# every encounter needs its own composition, so it defaults on.
BREAKDOWN <- TRUE

# cfg$routing$max_trip_duration is 90 and deliberately loose for the research
# pass, where 04 applies tighter cutoffs post-hoc. Raise it here if your
# catchment has patients beyond it: anything over the cutoff is absent from the
# result and lands as "unreachable", which is a finding, not a gap — but only if
# the cutoff was a real one.
MAX_TRIP_MIN <- cfg$routing$max_trip_duration

# Appointment times are floored to this grid before deduplication: two
# encounters in the same slot share an answer, and one r5r call serves both.
# This is the main runtime lever. cfg$routing$arrival_minute_step is 2, which is
# right for 03's temporal pass and far too fine here.
ROUND_MINUTES <- 15L

# --- route geometry -----------------------------------------------------------
# The matrix functions return travel times only - no shapes. Geometry comes from
# detailed_itineraries(), which is pairwise rather than all-to-all and is an
# order of magnitude slower, so this is opt-in and selective by design. Running
# it over a 17k extract is not the intended use.
#
# Pick trips one of two ways:
#   GEOMETRY_IDS              explicit COL_ID values - the usual case, when you
#                             want to see a specific patient's journey
#   GEOMETRY_SAMPLE_PER_SITE  n routed encounters per site, for eyeballing
#                             whether results look sane
# Leave both off (NULL / 0) to skip the section entirely.
#
# Only routable encounters are eligible either way — an unreachable trip has no
# itinerary and therefore no shape. With a low reachable rate that pool is much
# smaller than the extract, and it will contain walk-only trips, which are
# exactly the ones worth looking at on a map.
GEOMETRY_IDS             <- NULL
GEOMETRY_SAMPLE_PER_SITE <- 0L

# TRUE picks trips at evenly spaced points through each site's travel-time
# distribution — short, typical, long — instead of uniformly at random. Three
# random draws from a right-skewed distribution routinely land in a clump and
# tell you nothing about the range. FALSE gives a plain random sample.
GEOMETRY_SAMPLE_SPREAD   <- TRUE

GEOMETRY_MAX_TRIPS       <- 250L
OUT_GEOM_PATH            <- file.path(dirname(OUT_PATH), "encounters_routes.gpkg")

# --- car comparison -----------------------------------------------------------
# Free-flow drive time for the same origin-facility pairs, as context. One extra
# call — car routing is schedule-independent, so it does not repeat per slot.
# The research pass has its own car_baseline.rds, but that is for the synthetic
# origins and does not cover these patients.
CAR_COMPARISON <- TRUE

# ==============================================================================


# HELPERS
# ------------------------------------------------------------------------------
# burden_band(), count_agencies()/add_n_agencies() and haversine_km() are lifted
# verbatim from 04_aggregate.R. 04 does a full aggregation on source, so it
# cannot be sourced for its helpers alone. Worth factoring the three into
# R/00_utils.R and sourcing from both.

thr       <- cfg$measures$burden_thresholds
band_labs <- c(paste0("<=", thr[1]),
               paste0(thr[-length(thr)], "-", thr[-1]),
               paste0(">", thr[length(thr)]))

burden_band <- function(t) {
  out <- as.character(cut(t, breaks = c(0, thr, Inf), labels = band_labs, right = TRUE))
  out[is.na(t)] <- "unreachable"
  factor(out, levels = c(band_labs, "unreachable"))
}

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

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  dlon <- (lon2 - lon1) * pi / 180
  dlat <- (lat2 - lat1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Index of the single matching pattern, or NA if none or more than one match.
# The count comes back alongside so the caller can tell those cases apart.
match_facility <- function(x) {
  hits <- lapply(x, function(s) {
    if (is.na(s)) return(integer(0))
    which(vapply(FACILITY_PATTERNS$pattern,
                 function(p) grepl(p, s, ignore.case = TRUE), logical(1)))
  })
  list(idx = vapply(hits, function(h) if (length(h) == 1L) h else NA_integer_,
                    integer(1)),
       n = lengths(hits))
}

# Timestamps out of a real extract arrive in whatever shape the export tool felt
# like. Two cases have to be separated before any format is applied:
#
#   ALREADY PARSED.  read_csv() auto-detects ISO-8601 and hands back POSIXct.
#     as.character() of that is "2025-03-14 14:20:00", which an American format
#     string cannot parse - every row goes NA and the run dies at "routable: 0"
#     with nothing obviously wrong upstream. Reformat, do not re-parse.
#
#   STILL TEXT.  Try each candidate format and keep whichever parses the most
#     rows, reporting the winner so a near-miss is visible rather than silent.
parse_encounter_dt <- function(x) {
  tz <- cfg$window$timezone

  if (inherits(x, "POSIXt")) {
    message("datetime: column already POSIXct; using its clock face as local time")
    return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"),
                      format = "%Y-%m-%d %H:%M:%S", tz = tz))
  }
  if (inherits(x, "Date"))
    stop("COL_DATETIME is a Date with no time component - an arrival-based ",
         "router needs a clock time")

  # A non-NA result is NOT evidence the format was right. strptime is lenient in
  # two ways that both produce a confident, wrong, 100%-parsing answer:
  #
  #   %Y accepts a 2-digit year.  "3/14/25" under "%m/%d/%Y" is year 25 AD. It
  #     parses every row exactly as well as the correct "%y" format does, so a
  #     plain non-NA count cannot tell them apart.
  #   Trailing text is ignored.  "02:20 PM" under "%H:%M" is 02:20, silently
  #     losing the afternoon on precisely the rows where it matters.
  #
  # So each candidate is scored on rows that parse AND survive both checks.
  parse_ok <- function(s, f) {
    p  <- suppressWarnings(as.POSIXct(s, format = f, tz = tz))
    yr <- suppressWarnings(as.integer(format(p, "%Y")))
    ok <- !is.na(p) & !is.na(yr) & yr >= 1900L & yr <= 2100L
    if (!grepl("%p", f, fixed = TRUE))
      ok <- ok & !grepl("[AaPp]\\.?[Mm]\\.?\\s*$", s)
    p[!ok] <- NA
    list(p = p, n = sum(ok))
  }

  s    <- trimws(as.character(x))
  fmts <- DATETIME_FORMAT %||% DATETIME_FORMATS
  best <- NULL; best_n <- -1L; best_f <- NA_character_

  for (f in fmts) {
    r <- parse_ok(s, f)
    if (r$n > best_n) { best_n <- r$n; best <- r$p; best_f <- f }
  }

  message(sprintf("datetime: format '%s' parsed %d of %d rows (%.1f%%)",
                  best_f, best_n, length(s), 100 * best_n / max(length(s), 1)))
  if (best_n == 0L)
    stop("no candidate format parsed ANY row. First few raw values:\n  ",
         paste(utils::head(s[!is.na(s) & nzchar(s)], 3), collapse = "\n  "),
         "\n  Add the right format to DATETIME_FORMATS, or set DATETIME_FORMAT.")
  best
}

# arrival_travel_time_matrix() returns one row per Monte Carlo draw where a feed
# ships frequencies.txt (01 warns when one does). Collapse to the journey nearest
# the median rather than averaging the components independently, which would
# give an access, wait and ride time belonging to three different trips.
#
# NOTE: the helper column must not be named .d — R partial-matches `.d = expr`
# to mutate's `.data` argument and the expression is then evaluated outside the
# data mask, failing with a confusing "object not found" for a column that is
# plainly present.
collapse_draws <- function(x) {
  if (!nrow(x) || !"draw_number" %in% names(x)) return(x)
  x |>
    filter(!is.na(total_time)) |>
    group_by(from_id, to_id) |>
    mutate(.dev_med = abs(total_time - stats::median(total_time))) |>
    slice_min(.dev_med, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(-all_of(".dev_med"))
}


# NETWORK
# ------------------------------------------------------------------------------

heap_gb <- rJava::.jcall(
  rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime"),
  "J", "maxMemory") / 1024^3
message(sprintf("jvm heap: %.1f GB", heap_gb))
if (heap_gb < 4) stop("JVM heap is ", round(heap_gb, 1), " GB; 00_config.R was not sourced first")

net <- build_network(cfg$paths$network, overwrite = FALSE)

manifest_path <- file.path(cfg$paths$network, "network_manifest.json")
if (!file.exists(manifest_path))
  stop("no network_manifest.json in ", cfg$paths$network, " - run 01_network.R first")
manifest <- read_json(manifest_path)

# "start - end", written by 01 as the window over which every feed is
# simultaneously valid.
sw           <- trimws(unlist(strsplit(manifest$service_window, " - ", fixed = TRUE)))
common_start <- as.Date(sw[1])
common_end   <- as.Date(sw[2])
message("network service window: ", common_start, " to ", common_end)

scen <- cfg$scenarios[cfg$scenarios$scenario_label == SCENARIO, ]
stopifnot(nrow(scen) == 1L)
message(sprintf("scenario: %s (walk %.1f km/h, max walk %d min/leg), max trip %d min",
                scen$scenario_label, scen$walk_speed, scen$max_walk_time, MAX_TRIP_MIN))

stopifnot(all(FACILITY_PATTERNS$site_id %in% cfg$facilities$site_id))


# 1. READ AND VALIDATE
# ------------------------------------------------------------------------------

stopifnot(file.exists(IN_PATH))
enc <- read_csv(IN_PATH, show_col_types = FALSE)
message("\nread ", nrow(enc), " rows from ", basename(IN_PATH))

if (DEDUPE_ENCOUNTERS && !is.null(COL_ID)) {
  if (!COL_ID %in% names(enc))
    stop("COL_ID '", COL_ID, "' not found. Set COL_ID <- NULL or DEDUPE_ENCOUNTERS <- FALSE.")
  n_before <- nrow(enc)

  conflict <- enc |>
    group_by(.data[[COL_ID]]) |>
    filter(n() > 1) |>
    summarise(n_variants = n_distinct(paste(.data[[COL_LAT]], .data[[COL_LON]],
                                            .data[[COL_DATETIME]],
                                            .data[[COL_FACILITY]])),
              .groups = "drop") |>
    filter(n_variants > 1)

  if (nrow(conflict))
    message("WARNING: ", nrow(conflict), " encounter id(s) appear with differing ",
            "coordinates, facility or time. Keeping the first row of each. First few:\n  ",
            paste(head(conflict[[COL_ID]], 5), collapse = ", "))

  enc <- distinct(enc, .data[[COL_ID]], .keep_all = TRUE)
  message("unique encounters: ", nrow(enc), " (collapsed ", n_before - nrow(enc),
          " repeated row(s))")
}

miss <- setdiff(c(COL_LAT, COL_LON, COL_DATETIME, COL_FACILITY), names(enc))
if (length(miss))
  stop("columns not found in the extract: ", paste(miss, collapse = ", "),
       "\n  available: ", paste(names(enc), collapse = ", "))

# Row order is the join key of last resort, so fix it before anything reorders.
enc$.row <- seq_len(nrow(enc))

enc <- enc |>
  mutate(.lat = suppressWarnings(as.numeric(.data[[COL_LAT]])),
         .lon = suppressWarnings(as.numeric(.data[[COL_LON]])),
         .dt  = parse_encounter_dt(.data[[COL_DATETIME]]))

message("\ndatetime interpretation (confirm these look right):")
print(head(data.frame(raw  = as.character(enc[[COL_DATETIME]]),
                      used = format(enc$.dt, "%Y-%m-%d %H:%M %Z")), 5))

# A swapped lat/lon is the commonest extract error and routes perfectly
# plausibly somewhere else entirely, so check the hemisphere, not just for NA.
enc <- enc |> mutate(
  .bad_coord = !is.finite(.lat) | !is.finite(.lon) |
    .lat < 24 | .lat > 50 | .lon > -66 | .lon < -126,
  .bad_time  = is.na(.dt))

if (any(enc$.bad_coord)) {
  message("WARNING: ", sum(enc$.bad_coord), " row(s) have missing or implausible ",
          "coordinates (expected continental US). First few:")
  print(head(enc[enc$.bad_coord, c(COL_LAT, COL_LON)], 3))
}
if (any(enc$.bad_time)) {
  message("WARNING: ", sum(enc$.bad_time), " row(s) failed to parse with format '",
          DATETIME_FORMAT, "'. First few:")
  print(head(enc[[COL_DATETIME]][enc$.bad_time], 3))
}


# 2. FACILITY MATCH
# ------------------------------------------------------------------------------
# Matched once per distinct string, not per row: a 20k extract usually carries a
# handful of distinct facility values.

fac_vals <- unique(as.character(enc[[COL_FACILITY]]))
fm       <- match_facility(fac_vals)
fac_map  <- tibble(.fval = fac_vals, .fidx = fm$idx, .nhit = fm$n)

if (any(fac_map$.nhit == 0)) {
  message("\nFACILITY VALUES MATCHING NO PATTERN:")
  print(as.data.frame(fac_map[fac_map$.nhit == 0, ".fval"]))
  if (STRICT_FACILITY_MATCH)
    stop("add a pattern to FACILITY_PATTERNS, or correct the extract.")
}
if (any(fac_map$.nhit > 1)) {
  message("\nFACILITY VALUES MATCHING MORE THAN ONE PATTERN:")
  print(as.data.frame(fac_map[fac_map$.nhit > 1, ".fval"]))
  if (STRICT_FACILITY_MATCH)
    stop("FACILITY_PATTERNS are not mutually exclusive. Tighten them - picking ",
         "the first match would assign the wrong coordinates.")
}

enc <- enc |>
  mutate(.fval = as.character(.data[[COL_FACILITY]])) |>
  left_join(select(fac_map, .fval, .fidx), by = ".fval", relationship = "many-to-one") |>
  mutate(site = FACILITY_PATTERNS$site_id[.fidx], .no_facility = is.na(site)) |>
  left_join(cfg$facilities |> select(site = site_id, site_name,
                                     .site_lon = lon, .site_lat = lat),
            by = "site", relationship = "many-to-one")

message("\nfacility matching:")
print(count(enc, site, site_name) |> as.data.frame())


# 3. MAP ENCOUNTER DATES ONTO THE SERVICE WINDOW
# ------------------------------------------------------------------------------
# One representative date per weekday, drawn from inside the common service
# window.
#
# Anchoring on a fixed Monday-to-Sunday week does not work: feeds routinely
# publish a Sunday-to-Saturday validity period, so a seven-day window that
# covers every weekday still overflows a Monday-anchored week by a day. Choosing
# per weekday removes that failure mode and needs no week-alignment at all.

window_dates <- seq(common_start, common_end, by = "day")
wday_of      <- function(d) as.POSIXlt(d)$wday          # 0 = Sunday

# Where a weekday occurs more than once, take the occurrence nearest
# cfg$window$analysis_date - the one day 01_network.R actually confirmed has
# active service on every feed.
wday_date <- as.Date(rep(NA_real_, 7L), origin = "1970-01-01")
for (w in 0:6) {
  cand <- window_dates[wday_of(window_dates) == w]
  if (length(cand))
    wday_date[w + 1L] <- cand[which.min(abs(as.numeric(cand - cfg$window$analysis_date)))]
}

message("\nservice window covers ", length(window_dates), " day(s); encounters map by weekday:")
print(data.frame(weekday     = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
                 routed_date = format(wday_date),
                 row.names   = NULL))

if (anyNA(wday_date))
  message("WARNING: the window does not cover ",
          paste(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")[is.na(wday_date)],
                collapse = ", "),
          ". Encounters on those weekdays cannot be mapped and are reported as ",
          "'unmappable_date'.\n  Rebuild with feeds spanning a full week to fix it.")

message("NOTE: 01_network.R confirmed active service on ", cfg$window$analysis_date,
        " only. The other dates above sit inside the common service\n",
        "      window but were not individually checked - eyeball them for ",
        "holidays before trusting a run.")

# Floor to the rounding grid. Flooring an ARRIVAL deadline makes the patient
# arrive at or before the appointment, which is the safe direction to be wrong.
floor_to <- function(t, m) as.POSIXct(floor(as.numeric(t) / (m * 60)) * (m * 60),
                                      origin = "1970-01-01", tz = cfg$window$timezone)

enc <- enc |>
  mutate(
    .arrive_raw = as.POSIXct(
      paste(wday_date[wday_of(as.Date(.dt, tz = cfg$window$timezone)) + 1L],
            format(.dt, "%H:%M:%S")),
      format = "%Y-%m-%d %H:%M:%S", tz = cfg$window$timezone),
    .arrive   = floor_to(.arrive_raw, ROUND_MINUTES),
    .bad_date = is.na(.arrive))

if (any(enc$.bad_date & !enc$.bad_time))
  message("WARNING: ", sum(enc$.bad_date & !enc$.bad_time), " encounter(s) fall on ",
          "a weekday the service window does not cover.")

routable <- !enc$.bad_coord & !enc$.bad_time & !enc$.bad_date & !enc$.no_facility

# Component-wise, because "routable: 0 of 19146" on its own says nothing about
# which of four independent checks failed, and the conjunction hides it. Rows
# can fail more than one check, so these do not sum to the total.
excl <- data.frame(
  check = c("bad_coordinates", "unparsed_datetime", "no_facility_match",
            "unmappable_date"),
  n     = c(sum(enc$.bad_coord), sum(enc$.bad_time), sum(enc$.no_facility),
            sum(enc$.bad_date)))
excl$pct <- sprintf("%.1f%%", 100 * excl$n / nrow(enc))

message("\nexcluded before routing (a row can fail more than one check):")
print(excl, row.names = FALSE)
message("\nroutable: ", sum(routable), " of ", nrow(enc))

if (sum(routable) == 0L)
  stop("nothing is routable. The table above says which check is responsible.\n",
       "  unparsed_datetime at 100% almost always means DATETIME_FORMATS does ",
       "not cover this\n  extract's timestamps - check the parse message above ",
       "and the raw values:\n  ",
       paste(utils::head(as.character(enc[[COL_DATETIME]]), 3), collapse = " | "))


# 4. DEDUPLICATE INTO JOBS
# ------------------------------------------------------------------------------
# arrival_travel_time_matrix() takes ONE arrival instant per call, so encounters
# are grouped by rounded appointment time and each group is one call.

enc <- enc |>
  mutate(.okey = paste(sprintf("%.6f", .lon), sprintf("%.6f", .lat), sep = "|"),
         .slot = format(.arrive, "%Y-%m-%dT%H:%M"),
         .rk   = paste(.okey, site, .slot, sep = "|"))

work <- enc |> filter(routable) |> distinct(.rk, .keep_all = TRUE) |>
  select(.rk, .okey, .slot, .arrive, .lon, .lat, site, .site_lon, .site_lat)

# Stable origin ids: r5r needs unique ids and long coordinate strings make the
# result tables unreadable.
okeys  <- sort(unique(work$.okey))
oid    <- setNames(sprintf("o%06d", seq_along(okeys)), okeys)
work$.oid <- unname(oid[work$.okey])
enc$.oid  <- unname(oid[enc$.okey])

slots <- split(work, work$.slot)
message("\nunique origin-facility-slot combinations: ", nrow(work),
        "  (", round(100 * (1 - nrow(work) / sum(routable)), 1),
        "% saved by deduplication at ", ROUND_MINUTES, "-minute resolution)")
message("distinct origins: ", length(okeys),
        " | arrival slots (one r5r call each): ", length(slots))
if (length(slots) > 1500)
  message("WARNING: that is a lot of calls. Widen ROUND_MINUTES.")


# 5. ROUTE
# ------------------------------------------------------------------------------
# One RDS per slot, same resume-by-file-existence pattern as 03_route.R. A slot
# that RAISED is not written: the next run retries it rather than replaying a
# JVM failure as a result.

route_dir <- file.path(cfg$paths$outputs, cfg$run_id, "encounters",
                       tools::file_path_sans_ext(basename(IN_PATH)))
dir.create(route_dir, recursive = TRUE, showWarnings = FALSE)

# A cached slot is only valid for the settings that produced it. Widening
# MAX_TRIP_MIN and re-running would otherwise replay the old answers from disk.
fp_path <- file.path(route_dir, "_fingerprint.rds")
fp_now  <- list(scenario = SCENARIO, breakdown = BREAKDOWN, max_trip = MAX_TRIP_MIN,
                round = ROUND_MINUTES, wday_date = as.character(wday_date),
                mode = cfg$routing$mode, egress = cfg$routing$mode_egress,
                max_rides = cfg$routing$max_rides,
                walk_speed = scen$walk_speed, max_walk = scen$max_walk_time,
                network_built = manifest$built_at)
if (file.exists(fp_path) && !identical(readRDS(fp_path), fp_now))
  stop("cached slots in ", route_dir, " were produced with different settings ",
       "or a different network.\n  Delete the directory and re-run.")
saveRDS(fp_now, fp_path)

facilities <- data.frame(id  = cfg$facilities$site_id,
                         lon = cfg$facilities$lon,
                         lat = cfg$facilities$lat,
                         stringsAsFactors = FALSE)

message("\nrouting ", length(slots), " arrival slots (arrive-by, breakdown = ",
        BREAKDOWN, ") ...")
started <- Sys.time()
parts   <- vector("list", length(slots))

for (i in seq_along(slots)) {
  g <- slots[[i]]
  f <- file.path(route_dir, paste0("slot_", gsub("[^0-9]", "", g$.slot[1]), ".rds"))

  if (file.exists(f)) { parts[[i]] <- readRDS(f); next }

  o <- g |> distinct(.oid, .keep_all = TRUE) |>
    transmute(id = .oid, lon = .lon, lat = .lat) |> as.data.frame()
  # Only the sites this slot actually needs: the call is all-to-all, so shipping
  # all five every time multiplies the work for nothing.
  d <- facilities[facilities$id %in% unique(g$site), , drop = FALSE]

  res <- arrival_travel_time_matrix(
    net,
    origins           = o,
    destinations      = d,
    mode              = cfg$routing$mode,
    mode_egress       = cfg$routing$mode_egress,
    arrival_datetime  = g$.arrive[1],
    max_trip_duration = MAX_TRIP_MIN,
    max_walk_time     = scen$max_walk_time,
    walk_speed        = scen$walk_speed,
    max_rides         = cfg$routing$max_rides,
    breakdown         = BREAKDOWN,
    draws_per_minute  = cfg$routing$draws_per_minute,
    n_threads         = cfg$routing$n_threads,
    progress          = FALSE)

  res <- collapse_draws(as_tibble(res))
  saveRDS(res, f)
  parts[[i]] <- res

  if (i %% 25 == 0 || i == length(slots)) {
    rate <- as.numeric(difftime(Sys.time(), started, units = "secs")) / i
    message(sprintf("  [%4d/%4d] %s  %6d rows   eta %.1f h",
                    i, length(slots), g$.slot[1], nrow(res),
                    (length(slots) - i) * rate / 3600))
  }
}

# r5r omits unreachable pairs entirely, so this is sparse. The join below
# restores the full encounter set and absent rows become unreachable — same
# reasoning as 04's expand.grid.
routed <- bind_rows(parts) |> distinct(from_id, to_id, .keep_all = TRUE)

# r5r's column set depends on `breakdown`, and an extract where nothing at all
# routed can come back with no columns to rename. Pin the contract here so
# everything downstream can assume these exist rather than testing for them.
if (!all(c("from_id", "to_id") %in% names(routed)))
  routed <- tibble(from_id = character(0), to_id = character(0))
for (nm in c("total_time", "access_time", "wait_time", "ride_time",
             "transfer_time", "egress_time"))
  if (!nm %in% names(routed)) routed[[nm]] <- NA_real_
if (!"n_rides" %in% names(routed))        routed$n_rides        <- NA_integer_
if (!"routes" %in% names(routed))         routed$routes         <- NA_character_
if (!"departure_time" %in% names(routed)) routed$departure_time <- NA_character_


# 6. CAR COMPARISON
# ------------------------------------------------------------------------------
# Schedule-independent, so one call covers every origin regardless of slot.

car <- NULL
if (CAR_COMPARISON) {
  car_file <- file.path(route_dir, "_car.rds")

  # The cache holds the NORMALISED table, not r5r's raw output. Saving the raw
  # table and transforming afterwards meant the cached branch and the fresh
  # branch could hand section 7 two different shapes, and any failure in the
  # transform left `car` holding whichever shape it started from - which surfaces
  # much later as an unhelpful "join columns must be present" error.
  car_cols <- c(".oid", "site", "car_minutes")

  if (file.exists(car_file)) {
    car <- readRDS(car_file)
    if (!all(car_cols %in% names(car))) {
      message("\ncar comparison: cached file has an unexpected shape (",
              paste(names(car), collapse = ", "), ") - recomputing")
      car <- NULL
    } else {
      message("\ncar comparison: cached (", nrow(car), " rows)")
    }
  }

  if (is.null(car)) {
    message("\nrouting car comparison ...")
    o_all <- work |> distinct(.oid, .keep_all = TRUE) |>
      transmute(id = .oid, lon = .lon, lat = .lat) |> as.data.frame()
    raw <- travel_time_matrix(
      net, origins = o_all, destinations = facilities, mode = "CAR",
      # The one date 01 confirmed has service. Car routing ignores the
      # schedule anyway, but keeping it in-window avoids a pointless edge case.
      departure_datetime = as.POSIXct(
        paste(cfg$window$analysis_date, "10:00:00"), tz = cfg$window$timezone),
      max_trip_duration = cfg$nearest$car_max_duration,
      n_threads = cfg$routing$n_threads, progress = FALSE) |> as_tibble()

    # Named rather than positional: r5r has changed this column across versions
    # (travel_time_p50 vs travel_time_p050), and picking "whatever column is
    # not an id" silently grabs the wrong one if the output ever gains a field.
    tt_col <- grep("^travel_time", names(raw), value = TRUE)
    if (!all(c("from_id", "to_id") %in% names(raw)) || !length(tt_col))
      stop("travel_time_matrix() returned columns this script does not ",
           "recognise:\n  ", paste(names(raw), collapse = ", "),
           "\n  Expected from_id, to_id and a travel_time* column.")

    car <- raw |> transmute(.oid = from_id, site = to_id,
                            car_minutes = as.numeric(.data[[tt_col[1]]]))
    saveRDS(car, car_file)
    message("car comparison: ", nrow(car), " rows")
  }

  stopifnot(all(car_cols %in% names(car)))
}


# 7. ASSEMBLE
# ------------------------------------------------------------------------------

out <- enc |>
  left_join(routed |> rename(.oid = from_id, site = to_id),
            by = c(".oid", "site"), relationship = "many-to-one")

if (!is.null(car)) {
  if (!all(c(".oid", "site") %in% names(car)))
    stop("the car table is missing its join keys (has: ",
         paste(names(car), collapse = ", "), ").\n  Delete ",
         file.path(route_dir, "_car.rds"), " and re-run, or set ",
         "CAR_COMPARISON <- FALSE.")
  out <- left_join(out, car, by = c(".oid", "site"), relationship = "many-to-one")
}

if (!"routes" %in% names(out)) out$routes <- NA_character_
out <- add_n_agencies(out)

out <- out |>
  mutate(
    transit_minutes = round(total_time, 1),
    reachable       = !is.na(total_time),
    burden          = burden_band(total_time),
    # walk-only trips come back with n_rides = 0, so a bare n_rides - 1 gives -1
    n_transfers     = if (BREAKDOWN) pmax(n_rides - 1L, 0L) else NA_integer_,
    # Asked for WALK + TRANSIT, R5 returns whatever is fastest, sometimes a long
    # walk. That is a correct travel time and a wrong answer to "what is transit
    # access like here", so it is flagged rather than buried.
    used_transit    = if (BREAKDOWN) !is.na(n_rides) & n_rides > 0L else NA,
    straight_km     = round(haversine_km(.lon, .lat, .site_lon, .site_lat), 2),
    encounter_datetime  = format(.dt, "%Y-%m-%d %H:%M"),
    routed_arrival      = format(.arrive, "%Y-%m-%d %H:%M"),
    routed_weekday      = format(.arrive, "%a"),
    analysis_date       = as.character(cfg$window$analysis_date),
    service_window      = manifest$service_window,
    scenario            = SCENARIO,
    # NA tests come before value tests: a never-attempted row carries NA
    # total_time from the join and NA comparisons fall through silently.
    # n_rides is all-NA when BREAKDOWN is FALSE, so ok_walk_only simply never
    # fires there rather than needing a separate branch.
    route_status = case_when(
      .bad_coord                      ~ "bad_coordinates",
      .bad_time                       ~ "unparsed_datetime",
      .no_facility                    ~ "no_facility_match",
      .bad_date                       ~ "unmappable_date",
      is.na(total_time)               ~ "unreachable",
      !is.na(n_rides) & n_rides == 0L ~ "ok_walk_only",
      TRUE                            ~ "ok"))

if (BREAKDOWN)
  out <- out |> mutate(
    across(all_of(c("access_time", "wait_time", "ride_time",
                    "transfer_time", "egress_time")), ~ round(.x, 1)),
    # The share of the journey spent not on a vehicle, which is usually what
    # makes a transit trip feel long. Guarded: a patient geocoded at the
    # facility routes in zero minutes and an unguarded divide writes Inf.
    out_of_vehicle_share = ifelse(
      is.finite(total_time) & total_time > 0,
      round((access_time + wait_time + transfer_time + egress_time) / total_time, 3),
      NA_real_))

if (!is.null(car))
  out <- out |> mutate(
    car_minutes    = round(car_minutes, 1),
    transit_excess = round(transit_minutes - car_minutes, 1),
    transit_ratio  = ifelse(is.finite(car_minutes) & car_minutes > 0,
                            round(transit_minutes / car_minutes, 2), NA_real_))

new_cols <- c("site", "site_name", "scenario",
              "encounter_datetime", "routed_arrival", "routed_weekday",
              "departure_time", "transit_minutes", "reachable", "burden",
              if (BREAKDOWN) c("access_time", "wait_time", "ride_time",
                               "transfer_time", "egress_time",
                               "out_of_vehicle_share", "n_rides", "n_transfers",
                               "used_transit"),
              "routes", "n_agencies", "straight_km",
              if (!is.null(car)) c("car_minutes", "transit_excess", "transit_ratio"),
              "route_status", "analysis_date", "service_window")
new_cols <- intersect(new_cols, names(out))

final <- out |> arrange(.row) |>
  select(all_of(names(enc)[!startsWith(names(enc), ".")]), all_of(new_cols))

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
write_csv(final, OUT_PATH)


# 8. ROUTE GEOMETRY (optional)
# ------------------------------------------------------------------------------
# detailed_itineraries() is the only r5r function that returns shapes, and it
# has NO arrive-by mode — it takes departure_datetime only. So the departure is
# reconstructed from the `departure_time` the arrival matrix already reported
# for each trip, and the journey is re-routed depart-at from there.
#
# That reconstruction is close but not guaranteed identical. R5 optimises
# depart-at and arrive-by differently, so a re-routed trip can occasionally
# differ from the one the matrix pass costed. time_window = 1 and
# shortest_path = TRUE pin it as tightly as the API allows, and
# duration_matches flags any row where the two disagree by more than a minute.
# Trust transit_minutes for analysis; treat the geometry as illustrative.

geom_sel <- NULL
if (!is.null(GEOMETRY_IDS) || GEOMETRY_SAMPLE_PER_SITE > 0L) {
  pool <- out |>
    filter(route_status %in% c("ok", "ok_walk_only"),
           !is.na(departure_time), nzchar(departure_time))

  geom_sel <- if (!is.null(GEOMETRY_IDS)) {
    if (is.null(COL_ID)) stop("GEOMETRY_IDS needs COL_ID set")
    found <- pool |> filter(.data[[COL_ID]] %in% GEOMETRY_IDS)
    missing <- setdiff(GEOMETRY_IDS, found[[COL_ID]])
    if (length(missing))
      message("NOTE: ", length(missing), " requested id(s) are not routed ",
              "encounters and have no geometry: ",
              paste(utils::head(missing, 5), collapse = ", "))
    found
  } else if (GEOMETRY_SAMPLE_SPREAD) {
    # Evenly spaced through the ordered travel times: for n = 3 that is roughly
    # the 17th, 50th and 83rd percentile, so you get a short, a typical and a
    # long journey rather than whatever three draws happened to land on.
    pool |> group_by(site) |> group_modify(function(d, ...) {
      d <- arrange(d, transit_minutes)
      if (nrow(d) <= GEOMETRY_SAMPLE_PER_SITE) return(d)
      probs <- (seq_len(GEOMETRY_SAMPLE_PER_SITE) - 0.5) / GEOMETRY_SAMPLE_PER_SITE
      d[unique(round(stats::quantile(seq_len(nrow(d)), probs, type = 1))), ]
    }) |> ungroup()
  } else {
    pool |> group_by(site) |> slice_sample(n = GEOMETRY_SAMPLE_PER_SITE) |> ungroup()
  }

  # A site with no routable encounters contributes nothing and would otherwise
  # do so silently — which reads as a bug rather than as the finding it is.
  if (GEOMETRY_SAMPLE_PER_SITE > 0L) {
    got <- count(geom_sel, site, name = "sampled")
    tally <- enc |> distinct(site, site_name) |> filter(!is.na(site)) |>
      left_join(count(pool, site, name = "routable"), by = "site") |>
      left_join(got, by = "site") |>
      mutate(routable = coalesce(routable, 0L), sampled = coalesce(sampled, 0L))
    message("\ngeometry sample pool (routable encounters only):")
    print(as.data.frame(tally), row.names = FALSE)
    if (any(tally$sampled == 0L))
      message("NOTE: ", sum(tally$sampled == 0L), " site(s) contributed no ",
              "geometry - no routable encounter to draw.")
    n_wo <- sum(geom_sel$route_status == "ok_walk_only")
    if (n_wo)
      message("NOTE: ", n_wo, " of ", nrow(geom_sel), " sampled trip(s) use no ",
              "transit at all. Those shapes are a walk, end to end.")
  }

  # One shape per distinct origin-site-slot: duplicate encounters would produce
  # byte-identical lines and a needlessly fat GeoPackage.
  n_pre     <- nrow(geom_sel)
  geom_sel  <- distinct(geom_sel, .rk, .keep_all = TRUE)
  if (n_pre > nrow(geom_sel))
    message("geometry: ", n_pre, " selected -> ", nrow(geom_sel),
            " distinct route(s)")

  if (nrow(geom_sel) > GEOMETRY_MAX_TRIPS) {
    message("geometry: ", nrow(geom_sel), " routes exceeds GEOMETRY_MAX_TRIPS (",
            GEOMETRY_MAX_TRIPS, "); taking the first ", GEOMETRY_MAX_TRIPS, ".")
    geom_sel <- utils::head(geom_sel, GEOMETRY_MAX_TRIPS)
  }
}

if (!is.null(geom_sel) && nrow(geom_sel)) {

  # departure_time is a clock time ("07:42:13"), not a datetime. A journey that
  # departs later in the day than it arrives crossed midnight, so it started the
  # previous day — subtracting the comparison handles that without a special case.
  geom_sel <- geom_sel |>
    mutate(.arr_tod  = format(.arrive, "%H:%M:%S"),
           .dep_date = as.Date(.arrive, tz = cfg$window$timezone) -
             (departure_time > .arr_tod),
           .depart   = as.POSIXct(paste(.dep_date, departure_time),
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = cfg$window$timezone))

  dep_groups <- split(geom_sel, format(geom_sel$.depart, "%Y-%m-%dT%H:%M:%S"))
  message("\nrouting geometry for ", nrow(geom_sel), " trip(s) across ",
          length(dep_groups), " departure instant(s) ...")

  legs <- vector("list", length(dep_groups))
  for (i in seq_along(dep_groups)) {
    g <- dep_groups[[i]]
    # Pairwise, so origins and destinations must be the same length and aligned
    # row for row. all_to_all = FALSE is what enforces that reading.
    o <- g |> transmute(id = .rk, lon = .lon,      lat = .lat)      |> as.data.frame()
    d <- g |> transmute(id = .rk, lon = .site_lon, lat = .site_lat) |> as.data.frame()

    legs[[i]] <- tryCatch(
      detailed_itineraries(
        net, origins = o, destinations = d,
        mode              = cfg$routing$mode,
        mode_egress       = cfg$routing$mode_egress,
        departure_datetime = g$.depart[1],
        time_window       = 1L,
        max_trip_duration = MAX_TRIP_MIN,
        max_walk_time     = scen$max_walk_time,
        walk_speed        = scen$walk_speed,
        max_rides         = cfg$routing$max_rides,
        shortest_path     = TRUE,
        all_to_all        = FALSE,
        drop_geometry     = FALSE,
        n_threads         = cfg$routing$n_threads,
        progress          = FALSE),
      error = function(e) {
        message("  departure ", format(g$.depart[1]), " failed: ",
                conditionMessage(e))
        NULL
      })

    if (i %% 20 == 0 || i == length(dep_groups))
      message(sprintf("  [%3d/%3d]", i, length(dep_groups)))
  }

  legs <- bind_rows(Filter(Negate(is.null), legs))

  if (!nrow(legs)) {
    message("geometry: no itineraries returned - nothing written")
  } else {
    key <- geom_sel |>
      select(.rk, any_of(COL_ID), site, site_name, routed_arrival,
             encounter_datetime, matrix_minutes = transit_minutes) |>
      distinct(.rk, .keep_all = TRUE)

    legs <- legs |>
      rename(.rk = from_id) |>
      left_join(key, by = ".rk", relationship = "many-to-one") |>
      mutate(duration_matches = abs(total_duration - matrix_minutes) <= 1) |>
      # any_of throughout: r5r's leg columns have shifted across versions, and a
      # missing one should not cost you the whole GeoPackage. .rk is kept as the
      # route key so legs belonging to one journey can be grouped in QGIS.
      select(route_key = .rk, any_of(COL_ID), any_of(c(
        "site", "site_name", "encounter_datetime", "routed_arrival",
        "departure_time", "option", "segment", "mode", "route",
        "segment_duration", "wait", "distance", "total_duration",
        "total_distance", "matrix_minutes", "duration_matches")),
        geometry)

    n_trips <- length(unique(legs$route_key))
    n_off   <- length(unique(legs$route_key[!legs$duration_matches]))
    if (n_off)
      message("NOTE: ", n_off, " of ", n_trips, " re-routed trip(s) differ from ",
              "the arrival-matrix time by more than a minute.\n      ",
              "See duration_matches - those shapes are approximate.")

    dir.create(dirname(OUT_GEOM_PATH), recursive = TRUE, showWarnings = FALSE)
    st_write(st_as_sf(legs), OUT_GEOM_PATH, layer = "route_legs",
             delete_dsn = TRUE, quiet = TRUE)
    message("geometry: wrote ", nrow(legs), " legs across ", n_trips,
            " trip(s) to ", OUT_GEOM_PATH)
  }
}


# 9. SUMMARY
# ------------------------------------------------------------------------------

message("\n", strrep("-", 68))
message("wrote ", nrow(final), " rows to ", OUT_PATH)
message(strrep("-", 68))

message("\nroute_status:")
print(count(final, route_status) |> as.data.frame())

ok <- filter(final, route_status %in% c("ok", "ok_walk_only"))
if (nrow(ok)) {
  message("\nby site:")
  print(ok |> group_by(site, site_name) |> summarise(
    n          = n(),
    median_min = round(median(transit_minutes, na.rm = TRUE), 1),
    p90_min    = round(as.numeric(quantile(transit_minutes, 0.9, na.rm = TRUE)), 1),
    median_km  = round(median(straight_km, na.rm = TRUE), 1),
    .groups = "drop") |> as.data.frame())

  message("\nburden bands (reachable within ", MAX_TRIP_MIN, " min):")
  print(count(final, burden) |> as.data.frame())

  if (BREAKDOWN) {
    message("\nmedian out-of-vehicle share: ",
            round(100 * median(ok$out_of_vehicle_share, na.rm = TRUE), 1),
            "%  (walking, waiting, transferring)")
    n_walk <- sum(ok$route_status == "ok_walk_only")
    if (n_walk)
      message("NOTE: ", n_walk, " encounter(s) used no transit - walking was faster. ",
              "Probably not\n      what you want in a transit-access denominator.")
  }

  if (!is.null(car))
    message("\nmedian transit vs car: ",
            round(median(ok$transit_ratio, na.rm = TRUE), 2), "x  (+",
            round(median(ok$transit_excess, na.rm = TRUE), 1), " min)")
}

failed <- filter(final, !route_status %in% c("ok", "ok_walk_only"))
if (nrow(failed)) {
  message("\nfirst few non-routed:")
  print(head(failed |> select(any_of(c(COL_ID, COL_FACILITY)), route_status), 5) |>
          as.data.frame())
}

message("\nEncounter dates were mapped onto ", common_start, " - ", common_end,
        " by weekday, preserving\ntime of day. Results describe TYPICAL service ",
        "for that weekday and hour, not\nthose specific dates. Scheduled times ",
        "only - no bunching, delays or crowding.")
message("\nSlot cache in ", route_dir, "\n  delete it before re-running a ",
        "different extract, or after changing CONFIG.")
