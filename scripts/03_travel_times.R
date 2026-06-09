# =============================================================================
# 03_travel_times.R
#
# WHAT THIS DOES:
#   1. Calculates transit travel time from each patient to each hospital,
#      across several departure times (so we can see variation).
#   2. Summarizes into one row per patient: their closest and (if it exists)
#      second-closest hospital, with timing stats. -> the patient_access table.
#
# This step is slow, so it SAVES the raw matrix and reloads it next time.
#
# HOW TO RUN (after 00 and 02):
#   source("scripts/03_travel_times.R")
# =============================================================================

library(r5r)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# --- Batch helper: run the matrix in chunks to keep Java memory stable -------
run_ttm_batch <- function(origins_df, destinations_df, dep_time, batch_size) {
  n <- nrow(origins_df)
  batches <- split(origins_df, ceiling(seq_len(n) / batch_size))

  map_dfr(batches, function(batch) {
    travel_time_matrix(
      r5r_network,
      origins            = batch,
      destinations       = destinations_df,
      mode               = routing_mode,        # "TRANSIT" (from settings)
      departure_datetime = dep_time,
      max_trip_duration  = max_trip_duration,
      max_walk_time      = max_walk_time,
      time_window        = time_window,
      walk_speed         = walk_speed,
      max_rides          = max_rides,
      percentiles        = travel_percentile
    )
  })
}

# --- 1. Calculate (only if not already saved) --------------------------------
if (file.exists(ttm_cache)) {
  cat("Loading saved travel times.\n")
  ttm_all_hours <- readRDS(ttm_cache)
} else {
  cat("Calculating travel times (this can take a while)...\n")

  ttm_all_hours <- map_dfr(departure_times, function(dep_time) {
    cat("  departure:", format(dep_time, "%Y-%m-%d %H:%M"), "\n")
    run_ttm_batch(patients_filtered_sf, hospitals_sf,
                  dep_time, ttm_batch_size) %>%
      mutate(
        departure_time = dep_time,
        departure_hour = hour(dep_time)
      )
  })

  saveRDS(ttm_all_hours, ttm_cache)
  cat("Saved travel times to", ttm_cache, "\n")
}

# --- 2. Clean output ----------------------------------------------------------
# r5r names the travel-time column "travel_time_p25" (25 = our percentile).
# Detect it instead of hard-coding so this still works if you change the
# percentile in settings.
tt_col <- grep("^travel_time", names(ttm_all_hours), value = TRUE)[1]

ttm_final <- ttm_all_hours %>%
  rename(patient_id = from_id, hospital_id = to_id) %>%
  mutate(travel_time = .data[[tt_col]],
         reachable   = !is.na(travel_time)) %>%
  select(patient_id, hospital_id, departure_time, departure_hour,
         travel_time, reachable)

# --- 3. Per patient-hospital summary across departure times ------------------
patient_hospital_summary <- ttm_final %>%
  filter(reachable) %>%
  group_by(patient_id, hospital_id) %>%
  summarise(
    avg_tt   = mean(travel_time,   na.rm = TRUE),
    med_tt   = median(travel_time, na.rm = TRUE),
    min_tt   = min(travel_time,    na.rm = TRUE),
    max_tt   = max(travel_time,    na.rm = TRUE),
    range_tt = max_tt - min_tt,
    .groups  = "drop"
  )

# --- 4. Rank hospitals per patient (1 = closest by median time) --------------
patient_ranked <- patient_hospital_summary %>%
  group_by(patient_id) %>%
  arrange(med_tt, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# --- 5. Build the wide patient_access table ----------------------------------
# Keep the top 2 hospitals per patient and spread them into columns.
patient_access <- patient_ranked %>%
  filter(rank <= 2) %>%
  select(patient_id, rank, hospital_id,
         avg_tt, med_tt, min_tt, max_tt, range_tt) %>%
  pivot_wider(
    id_cols     = patient_id,
    names_from  = rank,
    values_from = c(avg_tt, med_tt, min_tt, max_tt, range_tt, hospital_id),
    names_glue  = "{.value}_rank{rank}"
  )

# IMPORTANT (your requested error handling):
# If NO patient can reach a second hospital by transit, pivot_wider never
# creates the *_rank2 columns, and the rename() below would crash. So we add
# any missing rank2 columns as NA first. This makes the script robust whether
# or not second-closest hospitals exist in the data.
rank2_cols <- c("avg_tt_rank2", "med_tt_rank2", "min_tt_rank2",
                "max_tt_rank2", "range_tt_rank2", "hospital_id_rank2")
for (col in rank2_cols) {
  if (!col %in% names(patient_access)) patient_access[[col]] <- NA
}

patient_access <- patient_access %>%
  rename(
    closest_hospital = hospital_id_rank1,
    closest_avg_tt   = avg_tt_rank1,
    closest_med_tt   = med_tt_rank1,
    closest_min_tt   = min_tt_rank1,
    closest_max_tt   = max_tt_rank1,
    closest_range_tt = range_tt_rank1,
    second_hospital  = hospital_id_rank2,
    second_avg_tt    = avg_tt_rank2,
    second_med_tt    = med_tt_rank2,
    second_min_tt    = min_tt_rank2,
    second_max_tt    = max_tt_rank2,
    second_range_tt  = range_tt_rank2
  ) %>%
  mutate(
    gap_closest_second = second_avg_tt - closest_avg_tt,  # NA if no 2nd hospital
    day_variability    = closest_range_tt,
    overall_avg_tt     = closest_avg_tt
  )

# Report how many patients have a second reachable hospital (useful context).
n_with_second <- sum(!is.na(patient_access$second_hospital))
cat("patient_access built:", nrow(patient_access), "patients.\n")
cat(n_with_second, "of them have a 2nd hospital reachable by transit.\n")
if (n_with_second == 0) {
  cat("NOTE: no patient has a 2nd reachable hospital, so all 'second_*' and\n",
      "     'gap_closest_second' values are NA. Maps/plots using those will\n",
      "     be skipped automatically in 05_maps_and_plots.R.\n")
}
