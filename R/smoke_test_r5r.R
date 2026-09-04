################################################################################
# smoke_test_r5r.R
#
# Throwaway diagnostic. Answers four questions about arrival_travel_time_matrix()
# before 03_route.R gets written:
#
#   Q1  Does arrival_datetime accept a VECTOR of times, or only one?
#   Q2  What columns come back, with and without breakdown?
#   Q3  Are unreachable O-D pairs returned as NA rows, or omitted entirely?
#   Q4  How long does one full-size call take, and how much does breakdown cost?
#
# Standalone on purpose: does not source 00_config.R, so it runs regardless of
# the state of the config. Set NETWORK_DIR and go.
################################################################################

options(java.parameters = "-Xmx25G")
library(r5r)

NETWORK_DIR <- "C:/Users/kampfschua/Documents/mass-transit-routing/data/network"
ARRIVAL     <- as.POSIXct("2026-08-20 09:00:00", tz = "America/New_York")
N_TIMING    <- 5000L    # full-size origin count for the timing test
SEED        <- 1

# routing params held at the primary scenario
MODE       <- c("WALK", "TRANSIT")
MAX_DUR    <- 180L
MAX_WALK   <- 15L
WALK_SPEED <- 3.6
MAX_RIDES  <- 3L

net <- r5r_network


# POINTS
# ------------------------------------------------------------------------------

facilities <- data.frame(
  id  = c("CHOP_PHL", "CHOP_KOPH", "BucksCounty_ASC", "BrandyWine_ASC", "Voorhees_ASC"),
  lon = c(-75.193771, -75.408033, -75.223773, -75.526230, -74.976538),
  lat = c( 39.948230,  40.087951,  40.268991,  39.886848,  39.845454)
)

set.seed(SEED)
mock_origins <- function(n) {
  data.frame(
    id  = sprintf("o%05d", seq_len(n)),
    lon = runif(n, -75.28, -74.96),   # Philadelphia city bbox
    lat = runif(n,  39.87,  40.14)
  )
}

o_small <- mock_origins(20)

# One deliberately remote point, inside the OSM extract but far from any GTFS
# stop. If it comes back as an NA row, r5r pads unreachable pairs; if it is
# simply absent, 04 has to rebuild the complete grid the way the original did.
o_remote <- rbind(o_small, data.frame(id = "REMOTE", lon = -77.50, lat = 40.80))

route <- function(origins, arrival, breakdown) {
  arrival_travel_time_matrix(
    net,
    n_threads = 4, 
    origins           = origins,
    destinations      = facilities,
    mode              = MODE,
    arrival_datetime  = arrival,
    max_trip_duration = MAX_DUR,
    max_walk_time     = MAX_WALK,
    walk_speed        = WALK_SPEED,
    max_rides         = MAX_RIDES,
    breakdown         = breakdown,
    progress          = FALSE
  )
}

hr <- function(x) cat("\n", strrep("=", 70), "\n", x, "\n", strrep("=", 70), "\n", sep = "")


# Q2  COLUMNS AND CARDINALITY
# ------------------------------------------------------------------------------

hr("Q2a  breakdown = FALSE, 20 origins x 5 facilities, 1 arrival instant")
r_plain <- route(o_small, ARRIVAL, FALSE)
cat("class :", paste(class(r_plain), collapse = ", "), "\n")
cat("nrow  :", nrow(r_plain), " (100 = complete grid)\n")
cat("names :", paste(names(r_plain), collapse = ", "), "\n\n")
print(utils::head(r_plain, 3))

hr("Q2b  breakdown = TRUE, same inputs")
r_break <- route(o_small, ARRIVAL, TRUE)
cat("nrow  :", nrow(r_break), "\n")
cat("names :", paste(names(r_break), collapse = ", "), "\n\n")
print(utils::head(r_break, 3))

cat("\ncolumns added by breakdown:",
    paste(setdiff(names(r_break), names(r_plain)), collapse = ", "), "\n")
cat("`routes` present without breakdown:", "routes" %in% names(r_plain), "\n")


# Q3  UNREACHABLE PAIRS
# ------------------------------------------------------------------------------

hr("Q3  does an unroutable origin come back as NA rows, or vanish?")
r_remote <- route(o_remote, ARRIVAL, FALSE)
id_col <- if ("from_id" %in% names(r_remote)) "from_id" else names(r_remote)[1]
cat("origins in  :", nrow(o_remote), "\n")
cat("origins out :", length(unique(r_remote[[id_col]])), "\n")
cat("REMOTE returned:", "REMOTE" %in% r_remote[[id_col]], "\n")
cat("rows returned:", nrow(r_remote), " (105 = padded, fewer = omitted)\n")
cat("\n-> if omitted, 03 stores only reachable rows and 04 rebuilds the grid\n")


# Q1  DOES arrival_datetime VECTORIZE?
# ------------------------------------------------------------------------------
# The decisive check is row count, not absence of an error: a silent fallback to
# the first element would return exactly as many rows as the single-time call.

hr("Q1  vector of 3 arrival times vs 1")
three <- ARRIVAL + c(0, 60, 120)
v <- tryCatch(route(o_small, three, FALSE),
              error = function(e) conditionMessage(e))

if (is.character(v)) {
  cat("ERRORED:", v, "\n")
  cat("\n-> NOT vectorized. One call per arrival instant.\n")
} else {
  cat("nrow single :", nrow(r_plain), "\n")
  cat("nrow x3     :", nrow(v), "\n")
  cat("names       :", paste(names(v), collapse = ", "), "\n")
  if (nrow(v) > nrow(r_plain) * 1.5) {
    cat("\n-> VECTORIZED. One call per hour instead of per minute.\n")
    tcol <- intersect(c("arrival_datetime", "arrival_time", "departure_time"), names(v))
    if (length(tcol)) {
      cat("distinct values of", tcol[1], ":", length(unique(v[[tcol[1]]])), "\n")
    }
  } else {
    cat("\n-> SILENTLY USED ONE TIME. Treat as not vectorized.\n")
  }
}


# Q4  TIMING AT FULL SIZE
# ------------------------------------------------------------------------------

hr(paste0("Q4  timing: ", N_TIMING, " origins x 5 facilities, 1 arrival instant"))
o_big <- mock_origins(N_TIMING)

t_plain <- system.time(big_plain <- route(o_big, ARRIVAL, FALSE))[["elapsed"]]
cat(sprintf("breakdown = FALSE : %6.1f s   (%d rows)\n", t_plain, nrow(big_plain)))

t_break <- system.time(big_break <- route(o_big, ARRIVAL, TRUE))[["elapsed"]]
cat(sprintf("breakdown = TRUE  : %6.1f s   (%d rows)\n", t_break, nrow(big_break)))
cat(sprintf("breakdown costs %.1fx\n", t_break / t_plain))


# EXTRAPOLATION
# ------------------------------------------------------------------------------

hr("projected wall time")
plan <- function(label, n_calls, secs) {
  cat(sprintf("%-46s %5d calls  %6.1f h\n", label, n_calls, n_calls * secs / 3600))
}
# 8 arrival hours; 2-minute step = 30 instants per hour = 240 per scenario
plan("1 scenario, 2-min step, no breakdown",      240,     t_plain)
plan("2 scenarios, 2-min step, no breakdown",     480,     t_plain)
plan("4 scenarios, 2-min step, no breakdown",     960,     t_plain)
plan("2 scenarios, 1-min step, no breakdown",     960,     t_plain)
plan("hourly breakdown pass, 4 scenarios",         32,     t_break)

cat("\nIf arrival_datetime vectorized, divide the call counts by 30.\n")

saveRDS(
  list(plain = r_plain, breakdown = r_break, remote = r_remote,
       vectorized = v, t_plain = t_plain, t_break = t_break),
  file.path(dirname(NETWORK_DIR), "smoke_test_results.rds")
)
cat("\nsaved smoke_test_results.rds\n")
