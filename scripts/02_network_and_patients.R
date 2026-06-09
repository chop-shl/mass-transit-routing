# =============================================================================
# 02_network_and_patients.R
#
# WHAT THIS DOES:
#   1. Loads patients and hospitals as map points.
#   2. Builds the r5r routing network from the street map (.pbf) + GTFS.
#   3. Pulls the transit lines out of the network and buffers them to make a
#      "transit service area".
#   4. Keeps only patients who live inside that service area.
#
# BEFORE RUNNING: put one OSM street map (.osm.pbf) in data/network/ AND the
#   clean GTFS zips (from 01_get_gtfs.R). Download a .pbf from
#   https://download.geofabrik.de/
#
# HOW TO RUN:
#   source("scripts/00_settings.R")
#   source("scripts/02_network_and_patients.R")
# =============================================================================

library(r5r)
library(sf)
library(dplyr)
library(mapview)

# --- 1. Patients --------------------------------------------------------------
# PHI NOTE: real patient home locations. Only run with PHI access. Kept in
# memory; we don't write patient points to disk.
patients_object <- readRDS(patients_file)

patients_all <- patients_object %>%
  transmute(
    id  = patient_id,
    lon = origin_lon,
    lat = origin_lat
  )

patients_all_sf <- st_as_sf(patients_all, coords = c("lon", "lat"),
                            crs = 4326, remove = FALSE)

# --- 2. Hospitals (from settings) --------------------------------------------
hospitals_sf <- st_as_sf(hospitals, coords = c("lon", "lat"),
                         crs = 4326, remove = FALSE)

# --- 3. Build the routing network --------------------------------------------
# r5r reads the .pbf + GTFS zips in network_dir and builds a routable network.
# First build is slow; later runs reuse the cached network.dat in that folder.
r5r_network <- build_network(network_dir)

# --- 4. Transit service area --------------------------------------------------
# Pull transit lines, buffer them, and dissolve into one shape.
routes_sf <- transit_network_to_sf(r5r_network)$routes

service_area_sf <- routes_sf %>%
  st_transform(3857) %>%                      # meters-based CRS for buffering
  st_buffer(dist = service_area_buffer_m) %>%
  st_union() %>%
  st_as_sf() %>%
  st_transform(4326)                          # back to GPS coordinates

# --- 5. Keep only patients near transit --------------------------------------
patients_filtered_sf <- st_join(
  patients_all_sf, service_area_sf,
  join = st_intersects, left = FALSE
) %>%
  distinct(id, .keep_all = TRUE)

cat(nrow(patients_filtered_sf), "of", nrow(patients_all_sf),
    "patients are within", service_area_buffer_m, "m of transit.\n")

# --- 6. Quick look (optional) -------------------------------------------------
# Uncomment to view. These print maps to the Viewer pane.
# mapview(patients_all_sf, cex = 1, color = "black", col.regions = "black") + routes_sf
# mapview(patients_filtered_sf, cex = 1, color = "black", col.regions = "black")
