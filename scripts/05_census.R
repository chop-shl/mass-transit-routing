# =============================================================================
# 05_census.R
#
# WHAT THIS DOES:
#   1. Downloads census tract shapes (tigris) for your counties.
#   2. Figures out which tract each patient lives in, and averages the
#      travel-time numbers up to the tract level  -> ct_summary.
#   3. Downloads ACS data (tidycensus): poverty, % children, % no vehicle,
#      median income  -> regional_census.
#   4. Joins census + travel data into one table you can map:
#      regional_census_transit_joined.
#
# A NOTE ON TRACTS (worth knowing): averaging patient travel times up to a
#   tract loses individual detail. A tract that looks "far from care" is a
#   general signal, not a fact about any one patient (this is the ecological
#   fallacy -- don't read individual conclusions from tract averages).
#
# BEFORE RUNNING: you need a free Census API key saved as an environment
#   variable CENSUS_API_KEY. See the README.
#
# HOW TO RUN (after 00, 02, 03):
#   source("scripts/05_census.R")
# =============================================================================

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(tidyr)

# --- 1. Tract shapes for all counties ----------------------------------------
pa_tracts <- tracts(state = "42", county = pa_counties, cb = TRUE)
nj_tracts <- tracts(state = "34", county = nj_counties, cb = TRUE)
de_tracts <- tracts(state = "10", county = de_counties, cb = TRUE)

ct_all <- rbind_tigris(pa_tracts, nj_tracts, de_tracts) %>%
  st_transform(4326)

# --- 2. Assign each patient to a tract ---------------------------------------
patient_access_sf <- patients_filtered_sf %>%
  left_join(patient_access, by = c("id" = "patient_id")) %>%
  st_transform(4326)

patient_ct <- st_join(patient_access_sf, ct_all,
                      join = st_within, left = FALSE)

# small helper: most common value (for "which hospital is most common in tract")
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# --- 3. Average travel metrics up to the tract level -------------------------
# na.rm = TRUE means tracts where everyone lacks a 2nd hospital still produce a
# value for the closest-hospital columns; the second_* columns just come out NA.
ct_summary <- patient_ct %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(
    n_patients       = n(),
    closest_hospital = mode_value(closest_hospital),
    second_hospital  = mode_value(second_hospital),

    closest_avg_tt   = mean(closest_avg_tt,   na.rm = TRUE),
    closest_med_tt   = median(closest_med_tt, na.rm = TRUE),
    closest_min_tt   = min(closest_min_tt,    na.rm = TRUE),
    closest_max_tt   = max(closest_max_tt,    na.rm = TRUE),
    closest_range_tt = mean(closest_range_tt, na.rm = TRUE),

    second_avg_tt    = mean(second_avg_tt,    na.rm = TRUE),
    second_med_tt    = median(second_med_tt,  na.rm = TRUE),
    second_min_tt    = min(second_min_tt,     na.rm = TRUE),
    second_max_tt    = max(second_max_tt,     na.rm = TRUE),
    second_range_tt  = mean(second_range_tt,  na.rm = TRUE),

    gap_closest_second = mean(gap_closest_second, na.rm = TRUE),
    day_variability    = mean(day_variability,    na.rm = TRUE),
    overall_avg_tt     = mean(overall_avg_tt,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(GEOID = as.character(GEOID))

# When a whole tract has no values, mean/min/max of all-NA returns NaN/Inf.
# Convert those to clean NA so maps and plots handle them gracefully.
ct_summary <- ct_summary %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.nan(.) | is.infinite(.), NA, .)))

# --- 4. Pull ACS data ---------------------------------------------------------
acs_vars <- c(
  pov = "B17001_002", tot_pop = "B01003_001", med_inc = "B19013_001",
  no_veh = "B08201_002", hh_tot = "B08201_001",
  m_u5="B01001_003", m_5_9="B01001_004", m_10_14="B01001_005",
  m_15_17="B01001_006", m_18_19="B01001_007", m_20="B01001_008", m_21="B01001_009",
  f_u5="B01001_027", f_5_9="B01001_028", f_10_14="B01001_029",
  f_15_17="B01001_030", f_18_19="B01001_031", f_20="B01001_032", f_21="B01001_033"
)

get_regional_acs <- function(state, counties) {
  get_acs(geography = "tract", state = state, county = counties,
          variables = acs_vars, year = acs_year, geometry = TRUE)
}

regional_census_raw <- bind_rows(
  get_regional_acs("42", pa_counties),
  get_regional_acs("34", nj_counties),
  get_regional_acs("10", de_counties)
)

regional_census <- regional_census_raw %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    poverty_rate   = 100 * (pov / tot_pop),
    pct_children   = 100 * ((m_u5+m_5_9+m_10_14+m_15_17+m_18_19+m_20+m_21+
                             f_u5+f_5_9+f_10_14+f_15_17+f_18_19+f_20+f_21) / tot_pop),
    pct_no_vehicle = 100 * (no_veh / hh_tot),
    median_income  = med_inc
  ) %>%
  select(GEOID, NAME, poverty_rate, pct_children,
         pct_no_vehicle, median_income, geometry) %>%
  mutate(GEOID = as.character(GEOID))

# --- 5. Join census + travel data --------------------------------------------
regional_census_transit_joined <- regional_census %>%
  left_join(ct_summary, by = "GEOID")

cat("Census + travel data joined. Ready to map.\n")
cat("Object to use for maps/plots: regional_census_transit_joined\n")
