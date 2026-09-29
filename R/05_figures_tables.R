################################################################################
#                           05_figures_tables.R
#
#   Table 1    transit network composition and use      (Results, Routing Scale)
#              + compute_summary.csv for the Results compute sentence
#   Table 2    access by site: patients, patient-trips, travel time, slow walk
#   Figure     per-origin temporal range, CHOP_PHL      (Results 3.2)
#   Map A      facilities reachable by transit          (Results 3.1, 3.3)
#   Map C      where the appointment hour changes band  (Results 3.2)
#
# Everything writes to outputs/<run_id>/manuscript/. Composite assembly is
# done separately.
#
# Travel-time bands are <=30 / >30-45 / >45-60 / >60 minutes, set in SETTINGS
# and added to od_hour as od_hour$band. One definition, used by tables and
# figures alike; 04's median_burden is not used.
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

# Travel-time bands, in minutes. Each band includes its upper edge, so a
# 30.0-minute trip is "<=30" and a 30.5-minute trip is ">30-45". Methods
# should read: 30 minutes or less, more than 30 to 45, more than 45 to 60,
# and more than 60.
BANDS      <- c(30, 45, 60)
BAND_LABS  <- c("<=30", ">30-45", ">45-60", ">60")

MIN_REACH  <- 100L            # instants (of 240) an origin must be reachable at

CRS_MAP        <- 26918       # UTM 18N (NAD83); metres, so buffers are literal
VIEW_BUFFER_KM <- 12
COUNTY_SHP     <- NULL        # local .shp path, or NULL to fetch via tigris
MIN_WATER_KM2  <- 15          # NHD areas below this are ponds, not orientation
SHOW_ROUTES    <- TRUE

# display names for the sites, used in Tables 1 and 2
site_labels <- c(CHOP_PHL        = "CHOP Main",
                 CHOP_KOPH       = "King of Prussia",
                 Voorhees_ASC    = "Voorhees",
                 BrandyWine_ASC  = "Brandywine",
                 BucksCounty_ASC = "Bucks County")



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
  band     = c("<=30" = "#3b2f6b", ">30-45" = "#7a6fb0", ">45-60" = "#f0a43c", ">60" = "#b5306e"),
  
  stable   = "#f0a43c",
  signal   = "#3b2f6b",
  burden   = c("#f7dcb0", "#f0a43c", "#b5306e", "#6d1440"),
  
  facility = "#b03a2e",
  halo     = "#ffffff",
  halo2    = "#ebf2f5"
)

# one colour per band; if BAND_LABS changes, pal$band has to change with it
stopifnot(length(BAND_LABS) == length(pal$band))
band_cols <- c(setNames(unname(pal$band), BAND_LABS), "unreachable" = pal$none)


# HELPERS
# ------------------------------------------------------------------------------

band_of <- function(t) {
  out <- as.character(cut(t, c(0, BANDS, Inf), labels = BAND_LABS, right = TRUE))
  out[is.na(t)] <- "unreachable"
  factor(out, levels = c(BAND_LABS, "unreachable"))
}

# travel-time band for every patient-trip; non-feasible trips are "unreachable"
od_hour$band <- band_of(od_hour$total_time)
table(od_hour$band[od_hour$scenario == PRIMARY])

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
# TABLE 1  transit network composition and use
################################################################################
# One row per agency: what it adds to the network (routes, stops) and how much
# feasible travel rode it. Compute is not in the table; it is summarised at the
# end of this section for the Results sentence.
#
# A "feasible trip" is one od_hour row with reachable == TRUE in the primary
# scenario, i.e. one patient x site x appointment hour.


# 1. Feeds and the agency each one is reported under ---------------------------
# SEPTA and NJ Transit each publish rail and bus as separate feeds; both
# roll up to one agency row.

feeds <- data.frame(
  feed   = c("septa_google_rail", "septa_google_bus", "njt_rail", "njt_bus",
             "patco", "amtrak"),
  agency = c("SEPTA", "SEPTA", "NJ Transit", "NJ Transit", "PATCO", "Amtrak")
)
feeds$zip <- file.path(cfg$paths$network, paste0(feeds$feed, ".zip"))

file.exists(feeds$zip)   # all should be TRUE


# 2. Study area ------------------------------------------------------------------
# Routes and stops are counted inside the box around the patient origins and
# the sites. Without this, Amtrak and NJ Transit would be counted system-wide.

origins_raw <- read.csv(cfg$origins$file, colClasses = "character")
lon <- c(as.numeric(origins_raw[[cfg$origins$lon_field]]), cfg$facilities$lon)
lat <- c(as.numeric(origins_raw[[cfg$origins$lat_field]]), cfg$facilities$lat)
study_box <- c(xmin = min(lon, na.rm = TRUE), ymin = min(lat, na.rm = TRUE),
               xmax = max(lon, na.rm = TRUE), ymax = max(lat, na.rm = TRUE))
study_box


# 3. Routes and stops per feed ---------------------------------------------------
# Read one GTFS file straight out of the zip. Some agencies start the file
# with a byte-order mark, which glues junk onto the first column name
# (stop_id becomes "ï»¿stop_id"); the gsub strips it, as the map code does.

read_gtfs <- function(zip, file) {
  x <- read.csv(unz(zip, file), colClasses = "character", check.names = FALSE)
  names(x) <- gsub("^[^A-Za-z_]+", "", names(x))
  x
}

# GTFS route_type codes used by these feeds. 0 covers SEPTA's trolleys and
# NJ Transit's RiverLINE; 11 is trolleybus (SEPTA's trackless trolleys), which
# run on the road and are reported as bus.
mode_names <- c("0" = "Light rail", "1" = "Metro", "2" = "Rail", "3" = "Bus", "11" = "Bus")

network_rows <- list()
for (i in seq_len(nrow(feeds))) {
  z <- feeds$zip[i]
  message("reading ", feeds$feed[i])
  
  stops      <- read_gtfs(z, "stops.txt")
  routes     <- read_gtfs(z, "routes.txt")
  trips      <- read_gtfs(z, "trips.txt")
  stop_times <- read_gtfs(z, "stop_times.txt")   # the slow one
  
  # boarding stops (location_type blank or 0) inside the study area
  if (!"location_type" %in% names(stops)) stops$location_type <- ""
  in_box <- stops |>
    filter(location_type %in% c("", "0"),
           as.numeric(stop_lon) >= study_box["xmin"], as.numeric(stop_lon) <= study_box["xmax"],
           as.numeric(stop_lat) >= study_box["ymin"], as.numeric(stop_lat) <= study_box["ymax"])
  
  # a rail station with several platforms counts once
  if (!"parent_station" %in% names(in_box)) in_box$parent_station <- ""
  station <- ifelse(in_box$parent_station == "", in_box$stop_id, in_box$parent_station)
  
  # routes with at least one trip serving an in-box stop
  trips_in_box  <- unique(stop_times$trip_id[stop_times$stop_id %in% in_box$stop_id])
  routes_in_box <- routes |>
    filter(route_id %in% trips$route_id[trips$trip_id %in% trips_in_box])
  
  network_rows[[i]] <- data.frame(
    agency   = feeds$agency[i],
    n_routes = nrow(routes_in_box),
    n_stops  = length(unique(station)),
    modes    = paste(unique(mode_names[routes_in_box$route_type]), collapse = ", ")
  )
}
network_rows <- bind_rows(network_rows)
network_rows

# one row per agency (SEPTA and NJ Transit have two feeds each)
network <- network_rows |>
  group_by(agency) |>
  summarise(routes = sum(n_routes),
            stops  = sum(n_stops),
            modes  = paste(unique(unlist(strsplit(modes, ", "))), collapse = ", "),
            .groups = "drop")
network


# 4. Which agencies each feasible trip rode --------------------------------------
# od_hour$routes lists the routes a trip rode, separated by "|". Each route ID
# starts with a prefix naming its network (septa_L1, njt_..., patco_...,
# amtrak_...). A trip with no transit leg is stored as "[WALK]".

prefix_agency <- c(septa  = "SEPTA",
                   njt    = "NJ Transit",
                   patco  = "PATCO",
                   amtrak = "Amtrak")

# A patient-trip is one patient x site x appointment hour. Every one was
# evaluated (5,000 x 5 x 8 = 200,000); only some had a feasible trip.
n_evaluated <- nrow(filter(od_hour, scenario == PRIMARY))
n_patients  <- n_distinct(od_hour$origin_id)
n_evaluated

feasible <- od_hour |>
  filter(scenario == PRIMARY, reachable) |>
  mutate(trip = row_number())
n_feasible <- nrow(feasible)
n_feasible

# one row per trip x route, then the route's prefix gives its agency
trip_agency <- feasible |>
  select(trip, site, routes) |>
  separate_rows(routes, sep = "\\|") |>
  filter(routes != "[WALK]") |>
  mutate(agency = prefix_agency[sub("_.*", "", routes)])

# any prefix not in prefix_agency shows up here as NA; this should be empty
filter(trip_agency, is.na(agency)) |> distinct(routes)

trip_agency <- distinct(trip_agency, trip, site, agency)

use_by_agency <- trip_agency |>
  group_by(agency) |>
  summarise(n_trips = n_distinct(trip), n_sites = n_distinct(site), .groups = "drop")
use_by_agency

# trips that rode no transit at all: the origin was within walking distance
walk_only <- filter(feasible, routes == "[WALK]")
nrow(walk_only)

# trips that rode more than one agency (SEPTA bus to SEPTA rail does not count)
n_multi <- trip_agency |> count(trip) |> filter(n > 1) |> nrow()
n_multi


# 5. Assemble and write Table 1 --------------------------------------------------
# The trip columns sit under a spanning header that states the full chain,
# "12,427 of 200,000 evaluated", so "% of feasible" cannot be misread as a
# share of all 200,000.

num <- function(x) format(x, big.mark = ",", trim = TRUE)
pct <- function(n) sprintf("%.1f", 100 * n / n_feasible)

tab1 <- network |>
  left_join(use_by_agency, by = "agency") |>
  mutate(n_trips = coalesce(n_trips, 0L), n_sites = coalesce(n_sites, 0L)) |>
  arrange(desc(n_trips)) |>
  transmute(Agency = agency,
            Mode   = modes,
            Routes = num(routes),
            Stops  = num(stops),
            n      = num(n_trips),
            `% of feasible` = pct(n_trips),
            Sites  = n_sites)

tab1 <- bind_rows(
  tab1,
  data.frame(Agency = "Walking only", Mode = "Walk", Routes = "", Stops = "",
             n = num(nrow(walk_only)), `% of feasible` = pct(nrow(walk_only)),
             Sites = n_distinct(walk_only$site), check.names = FALSE),
  data.frame(Agency = "All feasible", Mode = "",
             Routes = num(sum(network$routes)), Stops = num(sum(network$stops)),
             n = num(n_feasible), `% of feasible` = pct(n_feasible),
             Sites = n_distinct(feasible$site), check.names = FALSE)
)
tab1

trip_header <- sprintf("Feasible patient-trips: %s of %s evaluated (%.1f%%)",
                       num(n_feasible), num(n_evaluated), 100 * n_feasible / n_evaluated)
trip_header

write.csv(tab1, file.path(out_dir, "table1_network.csv"), row.names = FALSE)

# site x agency, for the Results sentence on which sites depend on which agency
site_agency <- trip_agency |>
  count(site, agency, name = "n_trips") |>
  left_join(count(feasible, site, name = "site_trips"), by = "site") |>
  mutate(pct_of_site_trips = round(100 * n_trips / site_trips, 1))
site_agency
write.csv(site_agency, file.path(out_dir, "table1_site_by_agency.csv"), row.names = FALSE)

# Footnote. Definitions that Methods already gives (patient-trip, 90-minute
# feasibility, service window) are left to Methods. The last two sentences
# back claims made in Results.

# share of Voorhees' feasible patient-trips that used NJ Transit
voorhees_njt <- site_agency |>
  filter(site == "Voorhees_ASC", agency == "NJ Transit") |>
  pull(pct_of_site_trips)
voorhees_njt                                      # 100 means every trip

# which sites the walking-only trips reached
walk_sites <- count(walk_only, site) |> arrange(desc(n))
walk_sites
walk_text <- paste(sprintf("%s (%d)", site_labels[walk_sites$site], walk_sites$n),
                   collapse = " and ")

# routing time for the population pass (both walking scenarios)
pop_minutes <- sum(telemetry$elapsed_s[telemetry$pass == "population"]) / 60
pop_minutes

tab1_note <- sprintf(paste(
  "Primary scenario (%.1f km/h). Agency rows can sum to more than 100%% because %s",
  "(%.1f%%) patient-trips rode more than one agency. Sites is the number of the %d",
  "sites reached. Routes and stops are those within the study area, summed across",
  "each agency's feeds. %s of feasible patient-trips to Voorhees used NJ Transit;",
  "walking-only trips reached %s. Routing both walking scenarios took %.0f minutes."),
  cfg$scenarios$walk_speed[cfg$scenarios$is_primary],
  num(n_multi), 100 * n_multi / n_feasible, n_fac,
  ifelse(voorhees_njt == 100, "All", sprintf("%.0f%%", voorhees_njt)),
  walk_text, pop_minutes)

kbl(tab1, format = "html", align = c("l", "l", rep("r", 5)),
    caption = "Table 1. Transit network composition and use of each agency by feasible patient-trips.") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  add_header_above(setNames(c(4, 3), c(" ", trip_header))) |>
  row_spec(nrow(tab1), bold = TRUE) |>
  footnote(general = tab1_note, general_title = "", threeparttable = TRUE) |>
  save_kable(file.path(out_dir, "table1_network.html"))

message("table 1 written")


# 6. Compute, for the Results sentence -------------------------------------------
# Pairs evaluated is origins x sites for every routing call. Itineraries is how
# many of those pairs had a feasible trip. Both passes are reported because the
# origin-range figure uses the temporal pass.

compute_summary <- telemetry |>
  group_by(pass) |>
  summarise(pairs_evaluated = sum(n_origins) * n_fac,
            itineraries     = sum(n_rows),
            minutes         = sum(elapsed_s) / 60,
            .groups = "drop")
compute_summary <- bind_rows(
  compute_summary,
  summarise(compute_summary, pass = "all", across(-pass, sum))
) |>
  mutate(pct_feasible = 100 * itineraries / pairs_evaluated,
         pairs_per_s  = pairs_evaluated / (minutes * 60))
compute_summary

write.csv(compute_summary, file.path(out_dir, "compute_summary.csv"), row.names = FALSE)


################################################################################
# TABLE 2  access by site
################################################################################
# Measurement decisions: one row per site, so the reader sees the health
# system's footprint rather than one flagship. Patients are the natural unit
# for "who can get there"; patient-trips (patient x site x hour) keep the table
# consistent with Table 1 and carry time. The slow-walk column shows that a
# routing assumption changes the answer.


# 1. Labels and scenarios ------------------------------------------------------


SLOW <- cfg$scenarios$scenario_label[!cfg$scenarios$is_primary]
SLOW                                                 # should be one label

walk_primary <- cfg$scenarios$walk_speed[cfg$scenarios$is_primary]
walk_slow    <- cfg$scenarios$walk_speed[!cfg$scenarios$is_primary]


# 2. One row per patient and site ----------------------------------------------
# hours_feasible: how many of the eight appointment hours had a feasible trip
# site_median: median travel time across those feasible hours (NA if none)

patient_site <- od_hour |>
  group_by(scenario, site, origin_id) |>
  summarise(hours_feasible  = sum(reachable),
            hours_evaluated = n(),
            site_median     = median(total_time[reachable]),
            .groups = "drop")

table(patient_site$hours_evaluated)                  # should all be 8


# 3. Site rows -----------------------------------------------------------------

primary_ps <- filter(patient_site, scenario == PRIMARY)
slow_ps    <- filter(patient_site, scenario == SLOW)
primary_ph <- filter(od_hour, scenario == PRIMARY)   # patient-trips

by_site <- primary_ps |>
  group_by(site) |>
  summarise(patients  = n(),
            any_hour  = sum(hours_feasible > 0),
            all_hours = sum(hours_feasible == hours_evaluated),
            .groups = "drop") |>
  left_join(primary_ph |>
              group_by(site) |>
              summarise(trips_evaluated = n(),
                        trips_feasible  = sum(reachable),
                        tt_median = median(total_time[reachable]),
                        tt_q1     = quantile(total_time[reachable], 0.25),
                        tt_q3     = quantile(total_time[reachable], 0.75),
                        .groups = "drop"),
            by = "site") |>
  left_join(slow_ps |>
              group_by(site) |>
              summarise(slow_any_hour = sum(hours_feasible > 0), .groups = "drop"),
            by = "site") |>
  arrange(desc(any_hour))
by_site


# 4. All-sites row -------------------------------------------------------------
# Any hour: a feasible trip to at least one site at some hour.
# All 8 hours: at least one site that is reachable at all eight hours.

all_sites <- primary_ps |>
  group_by(origin_id) |>
  summarise(any_hour  = any(hours_feasible > 0),
            all_hours = any(hours_feasible == hours_evaluated),
            .groups = "drop")

feasible_ph <- filter(primary_ph, reachable)

total_row <- tibble::tibble(
  site            = "All sites",
  patients        = nrow(all_sites),
  any_hour        = sum(all_sites$any_hour),
  all_hours       = sum(all_sites$all_hours),
  trips_evaluated = nrow(primary_ph),
  trips_feasible  = nrow(feasible_ph),
  tt_median       = median(feasible_ph$total_time),
  tt_q1           = quantile(feasible_ph$total_time, 0.25),
  tt_q3           = quantile(feasible_ph$total_time, 0.75),
  slow_any_hour   = slow_ps |> group_by(origin_id) |>
    summarise(r = any(hours_feasible > 0)) |> pull(r) |> sum()
)
total_row


# 5. Patient level (Panel B) --------------------------------------------------
# Among patients with at least one feasible trip: how many sites each can
# reach, and their lowest travel time. A patient's lowest travel time is the
# lowest of their per-site medians (site_median above), i.e. their typical
# trip to the best site, not their single best hour.

patients <- primary_ps |>
  filter(hours_feasible > 0) |>
  group_by(origin_id) |>
  summarise(n_sites = n(),
            lowest  = min(site_median),
            second  = if (n() > 1) sort(site_median)[2] else NA_real_,
            .groups = "drop") |>
  mutate(band = band_of(lowest))

n_reach <- nrow(patients)
n_reach                                   # should equal "All sites", Any hour
table(patients$n_sites)
table(patients$band)

gap_to_second <- patients |> filter(n_sites > 1) |> mutate(gap = second - lowest)
summary(gap_to_second$gap)


# 6. Assemble and write Table 2 ------------------------------------------------

num    <- function(x) format(x, big.mark = ",", trim = TRUE)
n_pct  <- function(n, d) sprintf("%s (%.1f)", num(n), 100 * n / d)
med_iq <- function(m, a, b) ifelse(is.na(m), "",
                                   sprintf("%.0f (%.0f-%.0f)", m, a, b))

# Panel A: one row per site, then all sites
tab2a <- bind_rows(by_site, total_row) |>
  transmute(Site = ifelse(site %in% names(site_labels), site_labels[site], site),
            `Any hour`    = n_pct(any_hour, patients),
            `All 8 hours` = n_pct(all_hours, patients),
            `n (% of evaluated)` = n_pct(trips_feasible, trips_evaluated),
            `Median, min (IQR)`  = med_iq(tt_median, tt_q1, tt_q3),
            `Any hour, slow walk` = n_pct(slow_any_hour, patients))

# Panel B: only the first data column is used; percentages are of n_reach
band_rows <- sapply(BAND_LABS, function(b) sum(patients$band == b))

panel_b_label <- c("Sites reachable", "1", "2 or more",
                   "Lowest travel time to any site",
                   paste(BAND_LABS, "min"),
                   "Minutes to second-fastest site, median (IQR)")
panel_b_value <- c("",
                   n_pct(sum(patients$n_sites == 1), n_reach),
                   n_pct(sum(patients$n_sites > 1),  n_reach),
                   "",
                   n_pct(band_rows, n_reach),
                   med_iq(median(gap_to_second$gap),
                          quantile(gap_to_second$gap, 0.25),
                          quantile(gap_to_second$gap, 0.75)))

tab2b <- data.frame(Site = panel_b_label, `Any hour` = panel_b_value,
                    `All 8 hours` = "", `n (% of evaluated)` = "",
                    `Median, min (IQR)` = "", `Any hour, slow walk` = "",
                    check.names = FALSE)

tab2 <- bind_rows(tab2a, tab2b)
tab2

write.csv(bind_rows(mutate(tab2a, Panel = "A"), mutate(tab2b, Panel = "B")),
          file.path(out_dir, "table2_access_by_site.csv"), row.names = FALSE)

tab2_note <- sprintf(paste(
  "Panel A percentages of patients are of the %s synthetic patients. A patient-trip",
  "is one patient, one site and one appointment hour; each site has %s evaluated and",
  "all sites together %s. Any hour: at least one feasible trip across the eight",
  "appointment hours. All 8 hours: a feasible trip at every hour (for all sites, at",
  "least one site reachable at all eight hours). A trip is feasible if an itinerary",
  "arrives within %d minutes. Travel times are medians and interquartile ranges",
  "across feasible patient-trips. Slow walk assumes %.1f km/h instead of %.1f km/h.",
  "Panel B percentages are of the %s patients with at least one feasible trip. A",
  "patient's lowest travel time is the lowest of their per-site median travel times;",
  "minutes to the second-fastest site is among the %s patients who could reach two",
  "or more sites."),
  num(total_row$patients), num(by_site$trips_evaluated[1]), num(total_row$trips_evaluated),
  cfg$routing$max_trip_duration, walk_slow, walk_primary,
  num(n_reach), num(nrow(gap_to_second)))

# Panel B rows listed under a sub-heading ("1", "2 or more", the bands) are
# indented one step further than the sub-headings themselves
indent_rows <- nrow(tab2a) + which(panel_b_label %in% c("1", "2 or more",
                                                        paste(BAND_LABS, "min")))

kbl(tab2, format = "html", align = c("l", rep("r", 5)),
    col.names = c("", "Any hour", "All 8 hours", "n (% of evaluated)",
                  "Median, min (IQR)", "Any hour"),
    caption = "Table 2. Transit access by site and by patient.") |>
  kable_styling(bootstrap_options = c("condensed"), full_width = FALSE) |>
  add_header_above(setNames(c(1, 2, 1, 1, 1),
                            c(" ",
                              sprintf("Patients reachable, %.1f km/h, n (%%)", walk_primary),
                              "Feasible patient-trips",
                              "Travel time",
                              sprintf("%.1f km/h, n (%%)", walk_slow)))) |>
  pack_rows(index = setNames(c(nrow(tab2a), nrow(tab2b)),
                             c("A. By site",
                               sprintf("B. Patients with a feasible trip (n = %s)", num(n_reach))))) |>
  add_indent(indent_rows) |>
  row_spec(nrow(tab2a), bold = TRUE) |>
  footnote(general = tab2_note, general_title = "", threeparttable = TRUE) |>
  save_kable(file.path(out_dir, "table2_access_by_site.html"))

message("table 2 written")


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
for (k in seq_along(BANDS)) {
  message(sprintf("  range crosses %d min: %d origins (%.1f%%)",
                  BANDS[k], sum(cross_mat[, k]), 100 * mean(cross_mat[, k])))
}
message(sprintf("  crosses any threshold: %d (%.1f%%)",
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
                     "Dashed lines are the 30-, 45- and 60-minute thresholds."),
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


message("\nall outputs in ", out_dir)
mapC <- ggplot() +
  base_layers() +
  geom_sf(data = mapC_dat, aes(colour = status), size = 0.85, alpha = 0.85) +
  overlay_layers() +
  ggspatial::annotation_scale(pad_x = unit(12.2,"cm"),
                              pad_y = unit(.1,"cm"),
                              height = unit(.2,"cm"),
                              bar_cols = c("#e8e8e8","#100819"),
                              text_family = "serif")+
  ggspatial::annotation_north_arrow(pad_x = unit(14.2,"cm"),
                                    pad_y = unit(14.8,"cm"),
                                    style = ggspatial::north_arrow_nautical(line_col = "gray10",
                                                                            fill = c("#e8e8e8","#100819"),
                                                                            text_family = "serif"))+
  coord_view +
  scale_colour_manual(values = c("Never reachable" = pal$none,
                                 "Stable band" = pal$stable,
                                 "Band changes across the day" = pal$signal),
                      name = NULL) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3))) +
  theme_map()

save_fig(mapC, "mapC_flip", w = 6.5, h = 6.8)
message("map C written")

message("\nall outputs in ", out_dir)