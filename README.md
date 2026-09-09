# Transit Access to Care

An arrival-framed public transit routing pipeline for measuring how patients reach care sites using public transit.

The pipeline takes geocoded patient addresses and routes each patient to every care site, at every hour of the day, over a multi-agency transit network. It answers questions like: *Can this patient reach any of our sites by transit? Which site is closest? How does that change across the day? Who has no transit access at all?*

---

## What the pipeline produces

Four tables, each at a different level of detail:

| Table | One row per | Answers |
|-------|-------------|---------|
| **1. patient × site × hour** | patient, site, arrival hour | What does this specific trip look like? |
| **2. patient × site** | patient, site | How good is this patient's access to this site across the day? |
| **3. patient** | patient | What are this patient's options overall? |
| **4. site** | site | Who can reach this site, how hard is the trip, and whose closest option is it? |

Routing is **arrival-framed**: for each target arrival hour, the pipeline finds the *latest* time a patient could leave home and still arrive on time.

All routing runs **locally on your machine**. No patient address is ever sent to an external service, which is what makes the workflow compatible with HIPAA and similar data governance rules.

---

## Before you start: what you need to prepare

Running this successfully takes some setup. Here is everything, in order.

### 1. Install R and the required packages

Install [R](https://cran.r-project.org/) (and optionally [RStudio](https://posit.co/download/rstudio-desktop/)). Then install the packages the script uses:

```r
install.packages(c("tidyverse", "r5r", "digest", "zip"))
```

### 2. Install Java

`r5r` runs on Java under the hood. You need **Java 21 (JDK)** installed.

- Download from [Adoptium Temurin](https://adoptium.net/) (free, works on Windows/Mac/Linux).
- After installing, restart R and confirm it is found:

```r
rJavaEnv::java_check_version_rjava()   # or simply run r5r and watch for Java errors
```

### 3. Download a street network (`.pbf` file)

`r5r` needs the road/sidewalk network for the walking parts of each trip. This comes from **OpenStreetMap**, as a `.pbf` file.

- Download an extract for your metro area from [Geofabrik](https://download.geofabrik.de/) or [BBBike](https://extract.bbbike.org/).
- Pick an area that **covers all your patients and all your care sites**
- Save the `.pbf` file into your **network directory** (`network_dir`).

### 4. Find the GTFS feeds for every transit agency in your region

**GTFS** is the standard format transit agencies use to publish their schedules. Each agency publishes its own feed as a `.zip` file at a public URL.

You need to find the feed URL for **every agency a patient might realistically use** to reach your sites. In a multi-agency region, a single trip can cross agencies (e.g. a local bus to a regional rail line).

Where to find feed URLs:

- The agency's own developer/open-data page (search *"[agency name] GTFS"* or *"[agency name] developer resources"*).
- Aggregators like [Mobility Database](https://mobilitydatabase.org/) or [Transitland](https://www.transit.land/) — searchable directories of GTFS feeds worldwide.

Put each feed name and URL into the `gtfs_feeds` list in the config (see below). Use the **direct download link** to the `.zip`.

> **Some feeds are nested** — a downloaded `.zip` that contains more `.zip` files inside it (for example, separate bus and rail feeds bundled together). The script automatically detects and unpacks these, so you don't need to handle it yourself.

### 5. Prepare your patient file

A CSV with, at minimum:

- a **patient ID** column,
- a **latitude** column,
- a **longitude** column.

> Patients must already be **geocoded** (have lat/lon). Rows with missing coordinates are automatically dropped before routing.

---

## Configuring the pipeline

Open the script and edit the `CONFIGURE` block. Every setting you need is here.

### Directories

```r
network_dir <- ".../Data/Network"        # holds your .pbf and downloaded GTFS feeds
hash_dir    <- ".../Data/Network/Hash"    # bookkeeping for change detection (auto-created)
```

The **network directory must already exist** and contain your `.pbf` file before you run. The hash directory is created automatically if missing.

### Patient file

```r
patient_file     <- ".../patients.csv"    # path to your geocoded patient CSV
patient_id_field <- "patient_id"          # your ID column name
lat_field        <- "lat"                 # your latitude column name
lon_field        <- "lon"                 # your longitude column name
```

### Care sites (destinations)

```r
destinations <- data.frame(
  id  = c("SITE_A", "SITE_B", ...),       # your site names
  lon = c(-75.19, -75.40, ...),           # site longitudes
  lat = c( 39.94,  40.08, ...)            # site latitudes
)
```

One row per care site. Add or remove rows for however many sites you have.

### Transit feeds

```r
gtfs_feeds <- c(
  agency_one = "https://.../feed.zip",
  agency_two = "https://.../feed.zip",
  ...
)
```

One entry per agency (see step 4 above).

### Routing parameters

These define what counts as a reasonable transit trip. **The defaults reflect a pediatric population and should be reviewed for your use case.**

| Parameter | Default | What it means | Consider changing if… |
|-----------|---------|---------------|------------------------|
| `mode` | `"TRANSIT"` | Travel modes allowed | You want walk-only or other modes |
| `max_trip_duration` | `60` | Longest trip to model, in minutes. Trips beyond this are treated as unreachable | Your region is more spread out (try 90–120) |
| `max_walk_time` | `15` | Longest walk to/from a stop, per leg, in minutes | Your population walks more or less |
| `walk_speed` | `3.6` | Walking speed in km/h | Your population is able to walk faster |
| `max_rides` | `3` | Max vehicles per trip (3 = up to 2 transfers) | You want to allow more/fewer transfers |
| `batch_size` | `1000` | Patients routed per batch. Affects memory use, not results | You hit memory limits (lower it) |

### Analysis window

```r
timezone         <- "America/New_York"    # your region's timezone
arrival_hours    <- 6:17                  # hours to test (6 = 6am ... 17 = 5pm)
analysis_weekday <- "Wednesday"           # representative weekday to analyze
```

Set `arrival_hours` to span your clinical day. The pipeline picks the analysis weekday closest to the middle of the date range your feeds are valid for.

### Java memory

```r
java_memory <- "-Xmx25G"                  # max memory r5r can use
```

`-Xmx25G` means 25 GB. Set this comfortably below your machine's total RAM. Large patient counts and dense networks need more; lower it if you have less RAM available.

---

## Running it

Once everything above is set:

1. Confirm your `.pbf` file is sitting in `network_dir`.
2. Open the script and run it top to bottom.

On the **first run**, `r5r` builds the transit network from your `.pbf` and GTFS feeds — this takes a few minutes and only happens once. It rebuilds automatically only when a feed actually changes.

The four output tables are created as objects in your R session (`patient_transit_arrival`, `patient_site_summary`, `patient_level`, `site_level_summary`).

---

## How it works (high level)

1. **Read patients** — load the CSV, standardize the ID/lat/lon columns, keep all other fields.
2. **Download GTFS** — fetch each agency feed, unpack any nested zips.
3. **Detect changes** — hash every network file so unchanged feeds aren't reprocessed on re-runs.
4. **Prefix routes by agency** — tag each route with its agency name (only for new/changed feeds), so the pipeline can count how many agencies a trip uses.
5. **Set the validity window** — find the date range where all feeds overlap, and pick the analysis date inside it.
6. **Build the network** — compile the `.pbf` + GTFS into a routable network.
7. **Route** — for every patient, site, and arrival hour, find the latest feasible departure and break the trip into walk/wait/ride/transfer components.
8. **Summarize** — roll the results up into the four tables.

Because results only reflect the schedules in the feeds you used, every output table is stamped with the **service window** — the range of dates those results are valid for. Transit schedules change several times a year, so a result is a snapshot valid for a bounded period, not a permanent property of an address.

---
