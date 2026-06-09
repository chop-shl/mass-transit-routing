# =============================================================================
# 06_maps_and_plots.R
#
# WHAT THIS DOES:
#   Makes maps and plots from regional_census_transit_joined. Instead of
#   copy-pasting map code, there are small reusable functions. You call them
#   with the column name you want.
#
#   Each function checks first whether the column actually has data. Columns
#   that are all NA (e.g. second-hospital stats when no patient can reach a
#   second hospital) are skipped with a message instead of crashing.
#
# HOW TO RUN (after 00 -> 05):
#   source("scripts/06_maps_and_plots.R")
#   then call the functions in the console (examples at the bottom).
#
# To see all columns you can use:  names(regional_census_transit_joined)
# =============================================================================

library(mapview)
library(leaflet)
library(ggplot2)
library(ggExtra)
library(bivariateLeaflet)
library(RColorBrewer)
library(sf)

# Shorter name to type.
dat <- regional_census_transit_joined

# --- Hospital overlay (matches your original style) --------------------------
hospitals_overlay <- mapview(
  hospitals_sf,
  color       = "black",
  col.regions = "#005587",
  cex         = 6,
  legend      = FALSE,
  layer.name  = "Care Sites"
)

# --- Guard: does a column have at least 2 real (non-NA) values? --------------
has_data <- function(data, column) {
  if (!column %in% names(data)) return(FALSE)
  sum(!is.na(sf::st_drop_geometry(data)[[column]])) >= 2
}

# --- Helper 1: choropleth map of ONE column ----------------------------------
make_map <- function(data, column, title = column) {
  if (!has_data(data, column)) {
    message("Skipping map '", title, "': '", column, "' has no data."); return(invisible())
  }
  is_num <- is.numeric(data[[column]])
  mapview(
    data,
    zcol          = column,
    layer.name    = title,
    col.regions   = if (is_num) brewer.pal(9, "YlOrRd") else NULL,
    na.color      = "#CCCCCC",
    alpha.regions = 0.75,
    lwd           = 0.1
  ) + hospitals_overlay
}

# --- Helper 2: bivariate map comparing TWO columns ---------------------------
make_bivariate_map <- function(data, column1, column2) {
  if (!has_data(data, column1)) {
    message("Skipping bivariate: '", column1, "' has no data."); return(invisible())
  }
  if (!has_data(data, column2)) {
    message("Skipping bivariate: '", column2, "' has no data."); return(invisible())
  }
  create_bivariate_map(data = data, var_1 = column1, var_2 = column2) %>%
    addCircleMarkers(
      data = hospitals_sf, radius = 6.5, color = "white",
      fillColor = "black", fillOpacity = 1, weight = 1.5,
      label = hospitals_sf$id, group = "Care Sites"
    ) %>%
    addLayersControl(
      overlayGroups = "Care Sites",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(metric = TRUE, imperial = TRUE))
}

# --- Helper 3: scatter of TWO columns with marginal densities ----------------
make_scatter <- function(data, x_column, y_column,
                         x_label = x_column, y_label = y_column) {
  if (!has_data(data, x_column)) {
    message("Skipping scatter: '", x_column, "' has no data."); return(invisible())
  }
  if (!has_data(data, y_column)) {
    message("Skipping scatter: '", y_column, "' has no data."); return(invisible())
  }
  df <- sf::st_drop_geometry(data)

  p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
                      size = n_patients, color = closest_hospital)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 8), name = "Number of Patients") +
    labs(x = x_label, y = y_label) +
    theme_minimal(base_size = 13) +
    theme(axis.title = element_text(face = "bold"),
          legend.position = "right",
          panel.grid.minor = element_blank())

  ggMarginal(p, type = "density", fill = "grey80", alpha = 0.7)
}

# =============================================================================
# EXAMPLES -- run these in the console. Change column names freely.
# Maps open in the Viewer pane; scatter plots open in the Plots pane.
# =============================================================================

# --- Single-column maps (your original map1..map5) ---------------------------
# make_map(dat, "gap_closest_second", "Travel Time Gap (2nd - 1st Closest)")
# make_map(dat, "day_variability",    "Within-Day Travel Time Variability")
# make_map(dat, "closest_med_tt",     "Median Travel Time to Closest Hospital")
# make_map(dat, "closest_hospital",   "Most Common Closest Hospital")
# make_map(dat, "n_patients",         "Number of Patients")

# --- Bivariate maps (equity lens) --------------------------------------------
# make_bivariate_map(dat, "pct_no_vehicle", "closest_avg_tt")
# make_bivariate_map(dat, "poverty_rate",   "closest_range_tt")
# make_bivariate_map(dat, "pct_children",   "closest_med_tt")

# --- Scatter plots -----------------------------------------------------------
# make_scatter(dat, "pct_no_vehicle", "closest_avg_tt",
#              "Households Without Vehicle (%)", "Avg Travel Time (min)")
# make_scatter(dat, "poverty_rate", "closest_range_tt",
#              "Poverty Rate (%)", "Travel Time Variation (min)")
# make_scatter(dat, "pct_children", "closest_med_tt",
#              "Children (%)", "Median Travel Time (min)")

cat("Map/plot functions loaded. Try, for example:\n")
cat('  make_map(dat, "closest_med_tt", "Median Travel Time to Closest Hospital")\n')
