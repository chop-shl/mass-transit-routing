# Mass Transit Access to Care

---

## Scripts:

| Script | Function |
|--------|--------------|
| `00_settings.R` | configure: paths, hospitals, routing options, dates. |
| `01_get_gtfs.R` | Downloads GTFS for r5r network. |
| `02_network_and_patients.R` | Builds routing network. |
| `03_travel_times.R` | Patient to care travel time metrics. |
| `04_detailed_routes.R` | *(Optional)* Patient to care routes. |
| `05_census.R` | Enriches with census data. |
| `06_maps_and_plots.R` | Loads map/plot functions. |

---

## Setup

### 1. Install R and RStudio
- R: https://cran.r-project.org/
- RStudio: https://posit.co/download/rstudio-desktop/

### 2. Install Java 21
In the RStudio console:
```r
install.packages("rJavaEnv")
rJavaEnv::rje_consent(provided = TRUE)
rJavaEnv::java_quick_install(version = 21)
```
Then **restart RStudio**.

### 3. Install R packages
```r
install.packages(c(
  "r5r", "sf", "dplyr", "tidyr", "purrr", "lubridate",
  "tidycensus", "tigris", "ggplot2", "ggExtra", "mapview",
  "leaflet", "RColorBrewer", "bivariateLeaflet"
))
```

### 4. Get a Census API key (free)
1. Request one at https://api.census.gov/data/key_signup.html — they email it to you.
2. Save it so R finds it automatically. In the console:
   ```r
   install.packages("usethis")
   usethis::edit_r_environ()
   ```
   This opens a file called `.Renviron`. Add this line (paste your real key):
   ```
   CENSUS_API_KEY=your_key_here
   ```
   Save the file and **restart RStudio**.
3. Confirm it worked:
   ```r
   Sys.getenv("CENSUS_API_KEY")   # should print your key, not ""
   ```

---

## Inputs

1. **Street map** — download a regional `.osm.pbf` and put it in
   `data/network/`.
2. **Patient file** — an `.rds` with columns `patient_id`, `origin_lon`,
   `origin_lat`, placed at `data/patients/sim_pts_v1.rds`. (Change the path in
   `00_settings.R` if yours differs.)

---

## Run Instructions

Open the project by double-clicking `transit-access.Rproj` (this points R at
the right folder — check with `getwd()`, it should end in `/transit-access`).

**Step 1 — Get GTFS transit data:**
```r
source("scripts/00_settings.R")
source("scripts/01_get_gtfs.R")
get_all_gtfs()
```
The feeds save to `data/network/`.

**Step 2 — Run the analysis:**
```r
source("scripts/02_network_and_patients.R")
source("scripts/03_travel_times.R")
source("scripts/05_census.R")
source("scripts/06_maps_and_plots.R")
```

**Step 3 (optional) — Draw individual routes:**
```r
source("scripts/04_detailed_routes.R")
```

The computationally heavy steps (`03`, `04`) save their results to `data/`, so re-running just
reloads them. To force a recalculation, delete the matching `.rds` file.

---

## Viewing maps and plots

Sourcing `06_maps_and_plots.R` loads the functions but doesn't draw anything.
Call them in the console. Maps open in the **Viewer** pane (bottom-right);
scatter plots open in the **Plots** pane.

```r

# see columns to map
names(dat)

# single-variable maps
make_map(dat, "closest_med_tt", "Median Travel Time to Closest Care")
make_map(dat, "n_patients",     "Number of Patients")

# two-variable equity maps
make_bivariate_map(dat, "pct_no_vehicle", "closest_avg_tt")

# scatter plots
make_scatter(dat, "poverty_rate", "closest_range_tt",
             "Poverty Rate (%)", "Travel Time Variation (min)")
             
```

---

## Note: GTFS dates

`r5r` only routes on dates that exist in the GTFS calendar. If your departure date
is outside the feeds' valid window. Find a good date:
```r
source("scripts/04_detailed_routes.R")   # loads check_gtfs_dates()
check_gtfs_dates()
```
Then update `departure_times` and `route_departure` in `00_settings.R`, delete
`data/ttm_all_hours.rds` and `data/detailed_routes.rds` (and the
`data/route_batches/` folder), and re-run from `03`.

---

## Folder layout

```
transit-access/
├── scripts/        the numbered scripts
├── data/
│   ├── network/    street map (.pbf) + transit GTFS zips
│   └── patients/   patient file
└── README.md
```