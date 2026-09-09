################################################################################
#                                01_network.R
#
# Assembles the routable network and validates it against the analysis date.
#
#   OSM sources  ->  merged (+ optionally clipped) .pbf
#   GTFS feeds   ->  downloaded raw, route ids prefixed, written to network dir
#   build_network()
#   network_manifest.json
#
# Source directories are read-only. Everything the router sees is written into
# cfg$paths$network by this script, so build_network() cannot pick up anything
# unexpected and the merged .pbf can never be fed back in as a source.
################################################################################

source(here::here("R", "00_config.R"))

library(r5r)
library(jsonlite)

for (d in cfg$paths) dir.create(d, recursive = TRUE, showWarnings = FALSE)


# HELPERS
# ------------------------------------------------------------------------------

# GTFS text files are frequently UTF-8 with a BOM. Without this the first
# column comes back named "i..route_id" and the prefixing below silently
# assigns to a new column instead of the real one.
read_gtfs_txt <- function(path) {
  read.csv(path, colClasses = "character", fileEncoding = "UTF-8-BOM")
}

# na = "" because GTFS parsers treat a literal "NA" as data, not as missing.
write_gtfs_txt <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "")
}

# Feed members are sometimes nested in a subdirectory, so match on basename
# rather than on the full entry path.
read_from_zip <- function(zip_path, target) {
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  hit <- entries[basename(entries) == target]
  if (length(hit) == 0) return(NULL)
  read.csv(unz(zip_path, hit[1]), colClasses = "character")
}

run_osmium <- function(args) {
  if (Sys.which(cfg$osm$osmium_bin) == "" && !file.exists(cfg$osm$osmium_bin)) {
    stop("osmium not found at '", cfg$osm$osmium_bin, "'")
  }
  status <- system2(cfg$osm$osmium_bin, args)
  if (status != 0) stop("osmium failed: ", paste(args, collapse = " "))
}


# STREET NETWORK
# ------------------------------------------------------------------------------

pbf_sources <- list.files(cfg$paths$osm_sources, pattern = "\\.pbf$", full.names = TRUE)
if (length(pbf_sources) == 0) {
  stop("no .pbf files found in ", cfg$paths$osm_sources)
}

merged_pbf <- file.path(cfg$paths$network, cfg$osm$merged_file)

if (cfg$osm$rebuild || !file.exists(merged_pbf)) {

  # Optional clip, applied per source before merging: cheaper and lower-memory
  # than merging full state extracts and clipping after.
  if (!is.null(cfg$osm$clip_bbox)) {
    bbox_arg <- paste(cfg$osm$clip_bbox[c("xmin", "ymin", "xmax", "ymax")], collapse = ",")
    clipped <- file.path(tempdir(), paste0("clip_", basename(pbf_sources)))
    for (i in seq_along(pbf_sources)) {
      run_osmium(c("extract", "--bbox", bbox_arg,
                   "--strategy", cfg$osm$clip_strategy,
                   "--overwrite", "-o", clipped[i], pbf_sources[i]))
    }
    to_merge <- clipped
  } else {
    to_merge <- pbf_sources
  }

  if (length(to_merge) == 1) {
    file.copy(to_merge, merged_pbf, overwrite = TRUE)
  } else {
    # merge, not cat: state extracts overlap at borders and cat would leave
    # duplicate object ids in the output.
    run_osmium(c("merge", "--overwrite", "-o", merged_pbf, to_merge))
  }
  message("street network: merged ", length(to_merge), " source(s) -> ", basename(merged_pbf))

} else {
  message("street network: reusing existing ", basename(merged_pbf))
}


# GTFS DOWNLOAD
# ------------------------------------------------------------------------------

# Corporate TLS inspection breaks libcurl certificate validation for some hosts
# (Amtrak in particular): the browser trusts the intercepting proxy's root via
# the Windows certificate store, libcurl ships its own CA bundle and does not.
# wininet routes through the Windows HTTP stack and therefore the Windows
# store. It is deprecated and warns on every call, but it works; the permanent
# fix is a CURL_CA_BUNDLE pointing at the corporate root, which lives outside
# this repo. Carried over from the original script.
if (.Platform$OS.type == "windows") {
  options(download.file.method = "wininet")
}

# Local files are named from names(cfg$gtfs$feeds), not basename(url): Amtrak
# ships a generic "GTFS.zip" that would collide with any other feed doing the
# same. Downloads land in a temp file and are moved on success, so a failed
# transfer cannot leave a truncated zip that later gets built into the network.
#
# Returns one or more raw file paths: a single URL can carry several feeds.
download_feed <- function(feed_key, url) {

  raw_dir <- cfg$paths$gtfs_raw
  key_pattern <- paste0("^", feed_key, "(_|\\.)")

  cached <- list.files(raw_dir, pattern = key_pattern, full.names = TRUE)
  if (!cfg$gtfs$refresh && length(cached) > 0) {
    message("gtfs: ", feed_key, " (cached, ", length(cached), " feed(s))")
    return(cached)
  }

  tmp <- tempfile(fileext = ".zip")
  tryCatch(
    download.file(url, destfile = tmp, mode = "wb", quiet = TRUE),
    error = function(e) stop("download failed for ", feed_key, ": ", conditionMessage(e))
  )

  contents <- utils::unzip(tmp, list = TRUE)$Name
  nested <- contents[grepl("\\.zip$", contents, ignore.case = TRUE)]

  # Clear previous copies first, so a feed that stops shipping a nested zip
  # cannot leave a stale one behind to be built into the network.
  if (length(cached) > 0) file.remove(cached)

  if (length(nested) == 0) {
    dest <- file.path(raw_dir, paste0(feed_key, ".zip"))
    file.copy(tmp, dest, overwrite = TRUE)
  } else {
    # SEPTA wraps google_bus.zip and google_rail.zip inside gtfs_public.zip.
    # Each nested zip is a feed in its own right and all of them are kept;
    # junkpaths because the nesting may sit under a subdirectory.
    utils::unzip(tmp, files = nested, exdir = tempdir(), junkpaths = TRUE)
    inner <- basename(nested)
    dest <- file.path(raw_dir, paste0(feed_key, "_", inner))
    file.copy(file.path(tempdir(), inner), dest, overwrite = TRUE)
  }

  unlink(tmp)
  message("gtfs: ", feed_key, " downloaded (", length(dest), " feed(s))")
  dest
}

feeds <- do.call(rbind, lapply(names(cfg$gtfs$feeds), function(k) {
  data.frame(feed_key = k,
             raw_file = download_feed(k, cfg$gtfs$feeds[[k]]),
             stringsAsFactors = FALSE)
}))

# Prefix = the config key up to its first underscore, which reproduces the
# original script's agency.txt behaviour without parsing agency.txt: njt_rail
# and njt_bus share one agency and collapse to "njt", and both nested SEPTA
# feeds collapse to "septa". Change this gsub to keep them separate.
feeds$prefix       <- gsub("[^A-Za-z0-9].*$", "", feeds$feed_key)
feeds$feed_name    <- tools::file_path_sans_ext(basename(feeds$raw_file))
feeds$network_file <- file.path(cfg$paths$network, paste0(feeds$feed_name, ".zip"))

stopifnot(!anyDuplicated(feeds$feed_name))


# PREFIX ROUTE IDS
# ------------------------------------------------------------------------------
# Route ids are only unique within a feed, so they are namespaced before the
# feeds are combined; r5r reports bare route ids in its `routes` column and the
# operator has to be recoverable from them.
#
# This ALWAYS runs on every feed, reading from gtfs_raw and writing to the
# network dir. Sources stay pristine, so a partial run can never leave a mix of
# prefixed and unprefixed feeds in the built network.

prefix_feed <- function(src_zip, dest_zip, prefix) {

  work <- file.path(tempdir(), paste0("gtfs_", tools::file_path_sans_ext(basename(dest_zip))))
  unlink(work, recursive = TRUE)
  dir.create(work, recursive = TRUE)
  # junkpaths: some feeds nest their .txt files inside a folder.
  utils::unzip(src_zip, exdir = work, junkpaths = TRUE)

  add_prefix <- function(x) paste0(prefix, "_", x)

  routes_path <- file.path(work, "routes.txt")
  routes <- read_gtfs_txt(routes_path)
  if (!"route_id" %in% names(routes)) stop("routes.txt has no route_id in ", basename(src_zip))
  routes$route_id <- add_prefix(routes$route_id)
  # r5r reports either the id or the short name, so prefix both and the
  # operator is recoverable either way.
  if ("route_short_name" %in% names(routes)) {
    routes$route_short_name <- add_prefix(routes$route_short_name)
  }
  write_gtfs_txt(routes, routes_path)

  # foreign keys into routes.txt
  trips_path <- file.path(work, "trips.txt")
  trips <- read_gtfs_txt(trips_path)
  trips$route_id <- add_prefix(trips$route_id)
  write_gtfs_txt(trips, trips_path)

  fare_path <- file.path(work, "fare_rules.txt")
  if (file.exists(fare_path)) {
    fare <- read_gtfs_txt(fare_path)
    if ("route_id" %in% names(fare)) {
      fare$route_id <- add_prefix(fare$route_id)
      write_gtfs_txt(fare, fare_path)
    }
  }

  transfers_path <- file.path(work, "transfers.txt")
  if (file.exists(transfers_path)) {
    transfers <- read_gtfs_txt(transfers_path)
    changed <- FALSE
    for (col in c("from_route_id", "to_route_id")) {
      if (col %in% names(transfers)) {
        transfers[[col]] <- ifelse(nzchar(transfers[[col]]),
                                   add_prefix(transfers[[col]]), transfers[[col]])
        changed <- TRUE
      }
    }
    if (changed) write_gtfs_txt(transfers, transfers_path)
  }

  if (file.exists(dest_zip)) file.remove(dest_zip)
  zip::zip(zipfile = dest_zip, files = list.files(work), root = work)
  unlink(work, recursive = TRUE)

  dest_zip
}

# Clear the network dir of feeds first: dropping a URL from cfg$gtfs$feeds
# should remove it from the network, not leave it behind to be rebuilt.
stale <- list.files(cfg$paths$network, pattern = "\\.zip$", full.names = TRUE)
if (length(stale) > 0) file.remove(stale)

for (i in seq_len(nrow(feeds))) {
  prefix_feed(feeds$raw_file[i], feeds$network_file[i], feeds$prefix[i])
  message("prefixed: ", feeds$feed_name[i], " -> ", feeds$prefix[i], "_*")
}


# SERVICE ON THE ANALYSIS DATE
# ------------------------------------------------------------------------------
# Resolves how many services each feed actually runs on cfg$window$analysis_date.
# A feed with zero active services drops out of the network silently, which
# looks like poor transit access rather than a broken input.

# as.POSIXlt$wday avoids weekdays(), which returns locale-dependent strings.
weekday_col <- c("sunday", "monday", "tuesday", "wednesday",
                 "thursday", "friday", "saturday")[as.POSIXlt(cfg$window$analysis_date)$wday + 1]
date_key <- format(cfg$window$analysis_date, "%Y%m%d")

feed_service <- function(zip_path) {

  calendar   <- read_from_zip(zip_path, "calendar.txt")
  exceptions <- read_from_zip(zip_path, "calendar_dates.txt")

  active <- character(0)
  span   <- c(NA_character_, NA_character_)

  if (!is.null(calendar)) {
    in_range <- calendar$start_date <= date_key & calendar$end_date >= date_key
    active <- calendar$service_id[in_range & calendar[[weekday_col]] == "1"]
    span <- c(min(calendar$start_date), max(calendar$end_date))
  }

  if (!is.null(exceptions)) {
    today  <- exceptions[exceptions$date == date_key, ]
    active <- union(active, today$service_id[today$exception_type == "1"])
    # exception_type 2 is service REMOVED — the original only read additions,
    # so a holiday could pass unnoticed.
    active <- setdiff(active, today$service_id[today$exception_type == "2"])
    added  <- exceptions$date[exceptions$exception_type == "1"]
    span   <- range(c(span, added), na.rm = TRUE)
  }

  # GTFS-native version string. The agencies serve "current" from a fixed URL,
  # so this is the only durable identifier for which feed a run actually used.
  info <- read_from_zip(zip_path, "feed_info.txt")
  version <- if (!is.null(info) && "feed_version" %in% names(info)) {
    as.character(info$feed_version[1])
  } else {
    NA_character_
  }

  list(
    n_active        = length(active),
    service_start   = span[1],
    service_end     = span[2],
    has_frequencies = !is.null(read_from_zip(zip_path, "frequencies.txt")),
    feed_version    = version
  )
}

service <- lapply(feeds$network_file, feed_service)
names(service) <- feeds$feed_name

for (k in names(service)) {
  message(sprintf("%-22s %5d services on %s   [%s to %s]%s",
                  k, service[[k]]$n_active, cfg$window$analysis_date,
                  service[[k]]$service_start, service[[k]]$service_end,
                  if (service[[k]]$has_frequencies) "  frequencies.txt" else ""))
}

dead <- names(service)[vapply(service, function(s) s$n_active == 0L, logical(1))]
if (length(dead) > 0) {
  stop("no service on ", cfg$window$analysis_date, " for: ", paste(dead, collapse = ", "),
       "\nPick a different cfg$window$analysis_date, or drop the feed.")
}

# Window over which every feed is simultaneously valid — the latest start and
# the earliest end. Reported alongside results as the period the estimates
# describe.
starts <- as.Date(vapply(service, function(s) s$service_start, character(1)), "%Y%m%d")
ends   <- as.Date(vapply(service, function(s) s$service_end,   character(1)), "%Y%m%d")
common_start <- max(starts, na.rm = TRUE)
common_end   <- min(ends,   na.rm = TRUE)
service_window <- paste(common_start, "-", common_end)

message("common service window: ", service_window)
if (cfg$window$analysis_date < common_start || cfg$window$analysis_date > common_end) {
  warning("analysis_date falls outside the common service window; ",
          "feeds still run that day individually, but they do not all overlap")
}

# r5r exposes no router seed, so a frequency-based feed makes results
# non-deterministic. Worth knowing before it shows up as unexplained variance.
if (any(vapply(service, function(s) s$has_frequencies, logical(1)))) {
  warning("frequency-based feed present: routing is Monte Carlo and not bit-reproducible")
}


# BUILD
# ------------------------------------------------------------------------------

# Confirm the JVM actually took the requested heap. Setting java.parameters
# after the JVM has started fails silently and the run dies hours later.
heap_gb <- rJava::.jcall(
  rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime"),
  "J", "maxMemory"
) / 1024^3
message(sprintf("jvm heap: %.1f GB (requested %s)", heap_gb, JAVA_HEAP))
if (heap_gb < 4) stop("JVM heap is ", round(heap_gb, 1), " GB; 00_config.R was not sourced first")

# Feeds were rewritten above on every run, so the network is rebuilt whenever
# they were refreshed, the pbf was remade, or no built network exists yet.
rebuild <- cfg$gtfs$refresh || cfg$osm$rebuild ||
  length(list.files(cfg$paths$network, pattern = "\\.dat$")) == 0

build_start <- Sys.time()
r5r_network <- build_network(cfg$paths$network, overwrite = rebuild)
build_secs  <- as.numeric(difftime(Sys.time(), build_start, units = "secs"))
message(sprintf("network %s in %.1f s", if (rebuild) "built" else "loaded", build_secs))


# MANIFEST
# ------------------------------------------------------------------------------

manifest <- list(
  run_id         = cfg$run_id,
  built_at       = format(Sys.time(), tz = "UTC", usetz = TRUE),
  analysis_date  = format(cfg$window$analysis_date),
  service_window = service_window,
  build_seconds  = floor(build_secs),
  environment = list(
    r                   = R.version$version.string,
    r5r                 = as.character(utils::packageVersion("r5r")),
    java_heap_requested = JAVA_HEAP,
    git_sha = tryCatch(
      system2("git", c("-C", shQuote(cfg$paths$root), "rev-parse", "HEAD"),
              stdout = TRUE, stderr = FALSE)[1],
      error = function(e) NA_character_
    )
  ),
  osm = list(
    sources = data.frame(
      file     = basename(pbf_sources),
      bytes    = file.size(pbf_sources),
      modified = format(file.mtime(pbf_sources), tz = "UTC", usetz = TRUE),
      row.names = NULL
    ),
    merged    = basename(merged_pbf),
    clip_bbox = cfg$osm$clip_bbox
  ),
  gtfs = data.frame(
    feed_key        = feeds$feed_key,
    feed_name       = feeds$feed_name,
    prefix          = feeds$prefix,
    feed_version    = vapply(service, function(s) s$feed_version, character(1)),
    bytes           = unname(file.size(feeds$network_file)),
    downloaded      = format(file.mtime(feeds$raw_file), tz = "UTC", usetz = TRUE),
    n_active        = vapply(service, function(s) s$n_active, integer(1)),
    service_start   = vapply(service, function(s) s$service_start, character(1)),
    service_end     = vapply(service, function(s) s$service_end, character(1)),
    has_frequencies = vapply(service, function(s) s$has_frequencies, logical(1)),
    row.names = NULL
  )
)

write_json(manifest, file.path(cfg$paths$network, "network_manifest.json"),
           auto_unbox = TRUE, pretty = TRUE, null = "null")

message("Done. 01_network.R run to completion. You have passed the first test, you may proceed.")
