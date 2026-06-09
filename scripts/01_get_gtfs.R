# =============================================================================
# 01_get_gtfs.R
#
# WHAT THIS DOES:
#   1. Downloads transit schedule (GTFS) zip files from each agency.
#   2. Looks inside each zip. Some agencies put zips INSIDE the zip
#      (a "nested" zip). r5r can't read those, so we pull the inner ones out.
#   3. Puts the final, r5r-ready zip files directly into data/network/ --
#      the same folder where your street map (.pbf) lives.
#
# You don't tell it which feeds are nested -- it checks each one itself.
#
# HOW TO RUN:
#   source("scripts/00_settings.R")
#   source("scripts/01_get_gtfs.R")
#   get_all_gtfs()
# =============================================================================

library(utils)   # download.file, unzip, zip -- all built into R

# --- The feeds we want. To add an agency, add a name + url line. -------------
gtfs_feeds <- list(
  amtrak   = "https://content.amtrak.com/content/gtfs/GTFS.zip",
  septa    = "https://www3.septa.org/developer/gtfs_public.zip",
  njt_rail = "https://www.njtransit.com/rail_data.zip",
  njt_bus  = "https://www.njtransit.com/bus_data.zip"
)

# --- Helper: does this zip contain other zip files inside it? ----------------
is_nested <- function(zip_path) {
  inside <- unzip(zip_path, list = TRUE)$Name
  any(grepl("\\.zip$", inside, ignore.case = TRUE))
}

# --- Helper: zip a folder of .txt files flat (txt files at the zip root) -----
zip_flat <- function(folder, out_zip) {
  if (file.exists(out_zip)) file.remove(out_zip)
  txt_files <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)

  old_wd <- getwd()
  setwd(folder)                                       # so stored paths are flat
  zip(zipfile = out_zip, files = basename(txt_files), flags = "-q")
  setwd(old_wd)
}

# --- Download one feed and put the cleaned result in data/network/ -----------
process_feed <- function(name, url) {
  cat("Processing:", name, "\n")

  raw_zip <- tempfile(fileext = ".zip")
  download.file(url, raw_zip, mode = "wb", quiet = TRUE)   # mode="wb" = binary

  work <- file.path(tempdir(), name)
  unlink(work, recursive = TRUE)
  dir.create(work, showWarnings = FALSE)
  unzip(raw_zip, exdir = work)

  if (is_nested(raw_zip)) {
    cat("  ->", name, "is nested. Extracting inner feeds.\n")
    inner_zips <- list.files(work, pattern = "\\.zip$",
                             full.names = TRUE, recursive = TRUE)
    for (iz in inner_zips) {
      inner_name <- tools::file_path_sans_ext(basename(iz))
      sub <- file.path(work, paste0("extracted_", inner_name))
      dir.create(sub, showWarnings = FALSE)
      unzip(iz, exdir = sub)
      out <- file.path(network_dir, paste0(name, "_", inner_name, ".zip"))
      zip_flat(sub, normalizePath(out, mustWork = FALSE))
      cat("  -> saved:", basename(out), "\n")
    }
  } else {
    out <- file.path(network_dir, paste0(name, ".zip"))
    zip_flat(work, normalizePath(out, mustWork = FALSE))
    cat("  -> saved:", basename(out), "\n")
  }
}

# --- Run it for every feed ---------------------------------------------------
get_all_gtfs <- function() {
  dir.create(network_dir, recursive = TRUE, showWarnings = FALSE)

  # Remove old GTFS zips so stale feeds don't mix with new ones.
  old <- list.files(network_dir, pattern = "\\.zip$", full.names = TRUE)
  if (length(old) > 0) {
    file.remove(old)
    cat("Removed", length(old), "old GTFS zip(s).\n")
  }

  for (name in names(gtfs_feeds)) process_feed(name, gtfs_feeds[[name]])
  cat("\nDone. GTFS feeds are ready in:", network_dir, "\n")
  cat("If you also have an .osm.pbf street map in that folder, you're set.\n")
}
