
# CONFIGURE
# --------------------------------------

# set directories
network_dir <- "C:/Users/adiwidjaja/Projects/072026_Transit_v2/Data/TEST/Network"
hash_dir <- "C:/Users/adiwidjaja/Projects/072026_Transit_v2/Data/TEST/Hash"

# set patient file details
patient_file <- "C:/Users/adiwidjaja/Projects/072026_Transit_v2/Data/Patients/patient_geocoded_df.csv"
patient_id_field <- "patient_id"
lat_field <- "lat"
lon_field <- "lon"

# set destinations
destinations <- data.frame(
  id  = c("CHOP_PHL", "CHOP_KOPH", "BucksCounty_ASC", "BrandyWine_ASC", "Voorhees_ASC"),
  lon = c(-75.19377064882546, -75.40803285265305, -75.22377300507655, -75.52623023023766, -74.97653800693676),
  lat = c( 39.94822977365352,  40.087951279294394, 40.26899110711235, 39.88684806204198,  39.84545427341185)
)
site_ids <- destinations$id


# set gtfs feeds
gtfs_feeds <- c(
  patco    = "https://rapid.nationalrtap.org/GTFSFileManagement/UserUploadFiles/13562/PATCO_GTFS.zip",
  amtrak   = "https://content.amtrak.com/content/gtfs/GTFS.zip",
  septa    = "https://www3.septa.org/developer/gtfs_public.zip",
  njt_rail = "https://www.njtransit.com/rail_data.zip",
  njt_bus  = "https://www.njtransit.com/bus_data.zip"
)

# set routing parameters
mode <- "TRANSIT"
max_trip_duration <- 60
max_walk_time <- 15
walk_speed <- 3.6
max_rides <- 3

# set batch run size
batch_size <- 1000

# set analysis window
timezone <- "America/New_York"
arrival_hours <- 6:17
analysis_weekday <- "Wednesday"

# set java memory for r5r
java_memory <- "-Xmx25G"


# LOAD LIBRARIES
# --------------------------------------------

options(java.parameters = java_memory)

library(r5r)
library(tidyverse)
library(digest)
library(zip)


# READ PATIENT TABLE
#---------------------------

# read patient csv file as text columns
patients <- read.csv(patient_file, colClasses = "character")

# rename id, lat, lon to standard names and keep every other original field
patients <- patients %>%
  rename(
    patient_id = all_of(patient_id_field),
    lat = all_of(lat_field),
    lon = all_of(lon_field)
  ) %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  filter(
    !is.na(lat),
    !is.na(lon)
  )

routed_patient_ids <- patients$patient_id
total_patients <- nrow(patients)


# GTFS DOWNLOADING
# ----------------------------------

# bypass SSL issue...
options(download.file.method = "wininet")

# function to download gtfs zips and handle nested gtfs zips
lapply(gtfs_feeds, function(feed) {
  
  # extract download file name
  file_name <- basename(feed)
  
  # download to network directory
  dest_path <- file.path(network_dir, file_name)
  
  # download
  download.file(url = feed, destfile = dest_path, mode = "wb", quiet="TRUE")
  
  # inspect contents of zip
  contents <- unzip(dest_path, list = TRUE)$Name
  
  # check for nested zip files
  nested_zips <- contents[grepl("\\.zip$", contents, ignore.case = TRUE)]
  
  # if nested zip files
  if (length(nested_zips) > 0) {
    
    # extract zip contents and bring back to parent directory
    unzip(dest_path, files = nested_zips, exdir = network_dir, junkpaths = TRUE)
    
    # remove original zip with nested content
    file.remove(dest_path)}
})

# list gtfs zips in network
gtfs_zip_files <- list.files(
  network_dir,
  pattern = "\\.zip$",
  full.names = TRUE #filepath
)


# CHECK IF NETWORK UPDATED
# -----------------------------------------------------------

# find network input files (gtfs zip or street pbf)
network_input_files <- list.files(
  network_dir,
  pattern = "\\.(zip|pbf)$",
  # keep file path
  full.names = TRUE
)


# hash each record
current_network_hashes <- data.frame(
  file_name = basename(network_input_files),
  file_hash = vapply(
    network_input_files,
    function(input_file) digest(file = input_file, algo = "sha256"),
    character(1)
  )
)

# create hash directory if it does not exist
if (!dir.exists(hash_dir)) {
  dir.create(hash_dir, recursive = TRUE)
}

# previous network hashes
network_hash_previous <- file.path(hash_dir, "network_file_hashes.csv")

# does previous network hash file exist?
if (file.exists(network_hash_previous)) {
  
  # read previous hash file if it exists
  previous_network_hashes <- read.csv(
    network_hash_previous,
    colClasses = c(
      file_name = "character",
      file_hash = "character"
    )
  )
  
  # if file exists then it is not the first run
  is_first_network_run <- FALSE
  
  # if file does not exist
} else {
  
  # create template network hash table
  previous_network_hashes <- data.frame(
    file_name = character(0),
    file_hash = character(0)
  )
  
  is_first_network_run <- TRUE
}

# compare previous hash with current hash by creating network_comparison table
network_comparison <- current_network_hashes %>%
  left_join(
    previous_network_hashes,
    by = "file_name",
    suffix = c("_current", "_previous")
  )

# flag network changes
network_comparison <- network_comparison %>%
  
  # create status column
  mutate(
    status = case_when(
      
      # 1. new network file (no previous hash record)
      is.na(file_hash_previous) ~ "new",
      # 2. changed network file (current hash does not match previous hash)
      file_hash_current != file_hash_previous ~ "changed",
      # 3. unchanged network file
      TRUE ~ "unchanged"
    )
  )

# list of files that are new or changed
files_changed <- network_comparison %>%
  filter(status %in% c("new", "changed")) %>%
  pull(file_name)

# check TRUE/FALSE for is_first_network_run
if (is_first_network_run) {
  
  # if TRUE: build network
  network_input_changed <- TRUE
  message("first run — building network")
  
  # if FALSE: check for changed files
} else {
  
  network_input_changed <- length(files_changed) > 0
  
  if (network_input_changed) {
    message(
      length(files_changed), " network files changed: ",
      paste(files_changed, collapse = ", ")
    )
    
  } else {
    message("network files unchanged")
  }
}

# write current network hash into network_hash_previous for next run
write.csv(
  current_network_hashes,
  network_hash_previous,
  row.names = FALSE
)



# PREFIX ROUTE IDS WITH AGENCY NAME
# -----------------------------------------------------------

prefix_gtfs_routes <- function(zip_path) {
  
  # unzip feed into a temp folder
  work_dir <- file.path(tempdir(), "gtfs_prefix")
  unlink(work_dir, recursive = TRUE)
  dir.create(work_dir, recursive = TRUE)
  unzip(zip_path, exdir = work_dir, junkpaths = TRUE)
  
  # get agency name from agency.txt
  agency <- read.csv(file.path(work_dir, "agency.txt"), colClasses = "character")
  agency_label <- agency$agency_name[1]
  
  # routes.txt: prefix route_id
  routes_txt <- read.csv(file.path(work_dir, "routes.txt"), colClasses = "character")
  routes_txt$route_id <- paste0(agency_label, "_", routes_txt$route_id)
  routes_txt$route_short_name <- paste0(agency_label, "_", routes_txt$route_short_name)
  write.csv(routes_txt, file.path(work_dir, "routes.txt"), row.names = FALSE, na = "")
  
  # trips.txt: prefix route_id (foreign key to routes.txt)
  trips_txt <- read.csv(file.path(work_dir, "trips.txt"), colClasses = "character")
  trips_txt$route_id <- paste0(agency_label, "_", trips_txt$route_id)
  write.csv(trips_txt, file.path(work_dir, "trips.txt"), row.names = FALSE, na = "")
  
  # fare_rules.txt: prefix route_id (foreign key to routes.txt) if feed has it
  fare_rules_path <- file.path(work_dir, "fare_rules.txt")
  if (file.exists(fare_rules_path)) {
    fare_rules_txt <- read.csv(fare_rules_path, colClasses = "character")
    fare_rules_txt$route_id <- paste0(agency_label, "_", fare_rules_txt$route_id)
    write.csv(fare_rules_txt, fare_rules_path, row.names = FALSE, na = "")
  }
  
  # rezip flat back over the original feed
  file.remove(zip_path)
  zip::zip(
    zipfile = zip_path,
    files = list.files(work_dir),
    root = work_dir
  )
  
  unlink(work_dir, recursive = TRUE)
}

# apply prefix function only to new feeds / changed feeds
for (zip_path in gtfs_zip_files) {
  if (basename(zip_path) %in% files_changed) {
    prefix_gtfs_routes(zip_path)
    message("prefixed routes for ", basename(zip_path))
  }
}


# GET FEED RANGE
# ------------------------------------------------------------------------

get_feed_service_range <- function(zip_path) {
  
  # get files
  files_in_zip <- unzip(zip_path, list = TRUE)$Name
  
  # find calendar or calendar_dates 
  calendar_entry       <- files_in_zip[basename(files_in_zip) == "calendar.txt"]
  calendar_dates_entry <- files_in_zip[basename(files_in_zip) == "calendar_dates.txt"]
  
  # initialize start and end date vector
  start_dates <- c()
  end_dates   <- c()
  
  # if calendar exists
  if (length(calendar_entry) > 0) {
    calendar <- read.csv(unz(zip_path, calendar_entry[1]), colClasses = "character")
    # store all dates
    start_dates <- c(start_dates, calendar$start_date)
    end_dates   <- c(end_dates,   calendar$end_date)
  }
  
  # if calendar_dates exists
  if (length(calendar_dates_entry) > 0) {
    calendar_dates <- read.csv(unz(zip_path, calendar_dates_entry[1]), colClasses = "character")
    # filter where service exists
    added_service <- calendar_dates[calendar_dates$exception_type == "1", ]
    # store all dates
    start_dates <- c(start_dates, added_service$date)
    end_dates   <- c(end_dates,   added_service$date)
  }
  
  # format dates
  start_dates <- as.Date(start_dates, format = "%Y%m%d")
  end_dates   <- as.Date(end_dates,   format = "%Y%m%d")
  
  # table of min start date and max end date
  data.frame(
    feed  = basename(zip_path),
    start = min(start_dates, na.rm = TRUE),
    end   = max(end_dates,   na.rm = TRUE)
  )
}

# apply function to all gtfs zips and combine results into one table
feed_ranges <- bind_rows(lapply(gtfs_zip_files, get_feed_service_range))

# maximum minimum start date and minimum maximum end date
common_start <- max(feed_ranges$start, na.rm = TRUE)
common_end <- min(feed_ranges$end, na.rm = TRUE)

# valid date range labels
feed_range_stamp <- paste0(
  format(common_start, "%Y%m%d"), "_", format(common_end, "%Y%m%d")
)
service_window <- paste0(common_start, " to ", common_end)


# SELECT ANALYSIS DATE
# -----------------------------------------------------------

# find middle date of common date range (deterministic)
middle_date <- common_start + (common_end - common_start) / 2

# find nearest chosen weekday (representative of weekday schedule)
offset <- 0

repeat {
  
  # check before and after
  forward <- middle_date + offset
  backward <- middle_date - offset
  
  # after:
  if (weekdays(forward) == analysis_weekday &
      forward >= common_start &
      forward <= common_end) {
    
    analysis_date <- forward
    break
  }
  
  # before:
  if (weekdays(backward) == analysis_weekday &
      backward >= common_start &
      backward <= common_end) {
    
    analysis_date <- backward
    break
  }
  
  # +/- 1 if no wednesday found
  offset <- offset + 1
}


# BUILD R5R NETWORK
# ------------------------------------

# build network
r5r_network <- build_network(network_dir, overwrite = network_input_changed)

# format to datetime
arrival_times <- as.POSIXct(
  paste0(analysis_date, " ", sprintf("%02d", arrival_hours), ":00:00"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = timezone
)

arrival_hour_labels <- format(arrival_times, "%H:%M")


# ROUTING HELPERS
# -----------------------------------------------

# format patient table for routing (id, lat, lon fields)
make_origins <- function(patient_batch) {
  data.frame(
    id = as.character(patient_batch$patient_id),
    lon = patient_batch$lon,
    lat = patient_batch$lat
  )
}

# batch handling for memory efficiency
batch_rows <- function(batch_number, size, total) {
  first_row <- (batch_number - 1) * size + 1
  last_row <- min(batch_number * size, total)
  first_row:last_row
}

# counting agencies used from routes used
count_agencies <- function(routes_string) {
  # if NA (no route) then 0
  if (is.na(routes_string)) {
    return(0)
  }
  # r5r uses | to separate routes
  parts <- unlist(strsplit(routes_string, "\\|"))
  # grab agency name (e.g. SEPTA_1 returns SEPTA)
  parts <- parts[grepl("_", parts)]
  # list all agency names collected from routes used
  agencies <- sub("_.*$", "", parts)
  # count unique agency names
  length(unique(agencies))
}


# ARRIVAL TIME MATRIX
# -----------------------------------------

number_of_batches <- ceiling(total_patients / batch_size)

# initialize result list
arrival_results <- list()
arrival_slot <- 1

# for each arrival hour in arrival times
for (i in seq_along(arrival_times)) {
  
  # get current arrival time
  current_arrival_time <- arrival_times[i]
  arrival_hour_label <- format(current_arrival_time, "%H:%M")
  
  # for each batch group of patients
  for (batch_number in 1:number_of_batches) {
    
    # define which patients to batch
    patient_batch <- patients[batch_rows(batch_number, batch_size, total_patients), ]
    
    # run batch through r5r function
    batch_result <- arrival_travel_time_matrix(
      r5r_network,
      origins = make_origins(patient_batch),
      destinations = destinations,
      mode = mode,
      arrival_datetime = current_arrival_time,
      max_trip_duration = max_trip_duration,
      max_walk_time = max_walk_time,
      walk_speed = walk_speed,
      max_rides = max_rides,
      breakdown = TRUE #decomposition stats
    )
    
    # add arrival hour to result table
    batch_result$arrival_hour <- arrival_hour_label
    
    # save results into list
    arrival_results[[arrival_slot]] <- batch_result
    arrival_slot <- arrival_slot + 1
  }
  
  message("routed arrival hour ", arrival_hour_label)
}


# TABLE 1: PATIENT x SITE x HOUR
# -----------------------------------------------------------

# combine all arrival results into one table
arrival_stacked <- bind_rows(arrival_results) %>%
  rename(patient_id = from_id, site = to_id)

# create table of every combination of patients x site x hour
arrival_grid <- expand.grid(
  patient_id = routed_patient_ids,
  site = site_ids,
  arrival_hour = arrival_hour_labels,
  stringsAsFactors = FALSE
)

# join r5r results to complete patients x site x hour table
patient_transit_arrival <- arrival_grid %>%
  left_join(arrival_stacked, by = c("patient_id", "site", "arrival_hour")) %>%
  mutate(
    # calculate transfers as number of rides minus 1 (e.g. 3 rides means 2 transfers)
    n_transfers = n_rides - 1,
    # calculate agencies as count of unique agencies
    n_agencies = sapply(routes, count_agencies, USE.NAMES = FALSE),
    # calculate flag if trip exists (for that patient for that site for that hour)
    possible_this_hour = if_else(is.na(total_time), FALSE, TRUE),
    # versioning labels
    analysis_date = analysis_date,
    service_window = service_window
  ) %>%
  select(
    # fields to keep
    patient_id, site, analysis_date, arrival_hour, departure_time,
    total_time, access_time, wait_time, ride_time, transfer_time,
    egress_time, routes, n_rides, n_transfers, n_agencies,
    possible_this_hour, service_window
  )


# TABLE 2: PATIENT x SITE
# -----------------------------------------------------------
# generated from table 1

# keep only valid patient x site x hour results
reachable_rows <- patient_transit_arrival %>%
  filter(possible_this_hour)

# aggregate stats for reachable rows
summary_reachable <- reachable_rows %>%
  group_by(patient_id, site) %>%
  summarise(
    # calculate median trip statistics
    median_total_time_when_reachable = median(total_time),
    median_access_time_when_reachable = median(access_time),
    median_wait_time_when_reachable = median(wait_time),
    median_ride_time_when_reachable = median(ride_time),
    median_transfer_time_when_reachable = median(transfer_time),
    median_egress_time_when_reachable = median(egress_time),
    median_rides_when_reachable = median(n_rides),
    median_transfers_when_reachable = median(n_transfers),
    median_agencies_when_reachable = median(n_agencies),
    # find best travel hour (for a patient for a site)
    best_appt_hour = arrival_hour[which.min(total_time)],
    best_appt_time = min(total_time),
    # find worst travel hour (for a patient for a site)
    worst_appt_hour = arrival_hour[which.max(total_time)],
    worst_appt_time = max(total_time),
    # calculate travel time range
    total_time_range = max(total_time) - min(total_time),
    .groups = "drop"
  )

# get count of how many valid trips a patient had to each site across all hours
summary_counts <- patient_transit_arrival %>%
  group_by(patient_id, site) %>%
  summarise(
    # count of reachable TRUE for each hour
    n_hours_reachable = sum(possible_this_hour),
    n_hours_tested = n(),
    .groups = "drop"
  )

# create summary table (from the two tables)
patient_site_summary <- summary_counts %>%
  left_join(summary_reachable, by = c("patient_id", "site")) %>%
  mutate(
    # calculate percent of analysis window that patient was able to reach that site
    pct_hours_reachable = n_hours_reachable / n_hours_tested,
    # calculate temporal variability measure of range to average total trip time
    time_range_pct_swing = total_time_range / median_total_time_when_reachable,
    # flag for tiers of accessibility (30, 45, 60)
    under_30 = median_total_time_when_reachable <= 30,
    between_30_45 = median_total_time_when_reachable > 30 & median_total_time_when_reachable <= 45,
    between_45_60 = median_total_time_when_reachable > 45 & median_total_time_when_reachable <= 60,
    over_60 = median_total_time_when_reachable > 60,
    # versioning
    analysis_date = analysis_date,
    service_window = service_window
  )


# TABLE 3: PATIENT
# -----------------------------------------------------------
# generated from table 2

# rank sites by median travel time
patient_reachable_sites <- patient_site_summary %>%
  # remove unreachable trips
  filter(!is.na(median_total_time_when_reachable)) %>%
  group_by(patient_id) %>%
  # rank each site (ties.method first so ranks stay whole numbers)
  mutate(site_time_rank = rank(median_total_time_when_reachable, ties.method = "first")) %>%
  ungroup()

# identify first closest site for each patient
first_closest <- patient_reachable_sites %>%
  filter(site_time_rank == 1) %>%
  select(patient_id,
         # keep site and median time to that site
         first_closest_site = site,
         first_closest_time = median_total_time_when_reachable)

# identify second closest site for each patient
second_closest <- patient_reachable_sites %>%
  filter(site_time_rank == 2) %>%
  select(patient_id,
         # keep site and median time to that site
         second_closest_site = site,
         second_closest_time = median_total_time_when_reachable)

# aggregate from patient x site to patient
patient_access <- patient_site_summary %>%
  group_by(patient_id) %>%
  summarise(
    # calculate how many sites are reachable for that patient
    reachable_count = sum(!is.na(median_total_time_when_reachable)),
    # calculate highest percent of reachable hours given all sites
    best_pct_hours_reachable = ifelse(
      reachable_count > 0, max(pct_hours_reachable, na.rm = TRUE), NA_real_
    ),
    # calculate number of sites in each accessibility tier
    n_sites_under_30 = sum(under_30, na.rm = TRUE),
    n_sites_30_45 = sum(between_30_45, na.rm = TRUE),
    n_sites_45_60 = sum(between_45_60, na.rm = TRUE),
    n_sites_over_60 = sum(over_60, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # calculate flag of if any site is reachable
  mutate(reachable_any = reachable_count > 0)

# combine tables for patient summary table (keeps all original patient fields)
patient_level <- patients %>%
  left_join(patient_access, by = "patient_id") %>%
  left_join(first_closest, by = "patient_id") %>%
  left_join(second_closest, by = "patient_id") %>%
  mutate(
    # calculate appropriate field values for patients without access
    reachable_count = ifelse(is.na(reachable_count), 0, reachable_count),
    reachable_any = ifelse(is.na(reachable_any), FALSE, reachable_any),
    n_sites_under_30 = ifelse(is.na(n_sites_under_30), 0, n_sites_under_30),
    n_sites_30_45 = ifelse(is.na(n_sites_30_45), 0, n_sites_30_45),
    n_sites_45_60 = ifelse(is.na(n_sites_45_60), 0, n_sites_45_60),
    n_sites_over_60 = ifelse(is.na(n_sites_over_60), 0, n_sites_over_60),
    # versioning
    analysis_date = analysis_date,
    service_window = service_window
  )


# TABLE 4: SITE
# -----------------------------------------------------------
# generated from table 3

# site nearest counts 
site_catchment <- patient_level %>%
  # if no first closest then no valid trip
  filter(!is.na(first_closest_site)) %>%
  # count patients assigned closest to each site
  count(first_closest_site, name = "n_patients_nearest") %>%
  rename(site = first_closest_site)

# combine tables for site summary table
site_level_summary <- patient_site_summary %>%
  # row for each site
  group_by(site) %>%
  summarise(
    # calculate site stats
    n_patients_tested = n(),
    n_patients_reachable = sum(!is.na(median_total_time_when_reachable)),
    median_total_time = median(median_total_time_when_reachable, na.rm = TRUE),
    median_access_time = median(median_access_time_when_reachable, na.rm = TRUE),
    median_wait_time = median(median_wait_time_when_reachable, na.rm = TRUE),
    median_ride_time = median(median_ride_time_when_reachable, na.rm = TRUE),
    median_transfer_time = median(median_transfer_time_when_reachable, na.rm = TRUE),
    median_egress_time = median(median_egress_time_when_reachable, na.rm = TRUE),
    median_transfers = median(median_transfers_when_reachable, na.rm = TRUE),
    median_agencies = median(median_agencies_when_reachable, na.rm = TRUE),
    median_pct_hours_reachable = median(pct_hours_reachable[!is.na(median_total_time_when_reachable)]),
    n_under_30 = sum(under_30, na.rm = TRUE),
    n_30_45 = sum(between_30_45, na.rm = TRUE),
    n_45_60 = sum(between_45_60, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_patients_reachable = n_patients_reachable / n_patients_tested) %>%
  left_join(site_catchment, by = "site") %>%
  mutate(n_patients_nearest = ifelse(is.na(n_patients_nearest), 0, n_patients_nearest))