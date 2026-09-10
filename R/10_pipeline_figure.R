################################################################################
#                            06_figure1_pipeline.R
#
# Figure 1. Pipeline schematic: inputs, the five scripts, and what each stage
# hands to the next.
#
# Layout lives in two data frames. To move a box, edit its x/y/w/h in `nodes`
# and the edges follow, because edge endpoints are computed from the node
# geometry rather than hardcoded. To add a stage, add a row to `nodes` and an
# edge to `edges`.
#
# Canvas is 100 wide x 70 tall; keep the output aspect close to that or the
# text spacing drifts.
#
# Standalone by design — does not source 00_config.R, so it renders without a
# built network. The palette mirrors 05_figures_tables.R.
################################################################################

library(ggplot2)
library(dplyr)

out_dir <- here::here("outputs", "figure1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Light fills with a saturated border keep the text black and survive
pal <- list(
  input   = list(fill = "#f2f1ee", line = "#8a8781"),
  network = list(fill = "#e4ebf4", line = "#4f74a8"),
  route   = list(fill = "#fdefd9", line = "#f0a43c"),
  analyse = list(fill = "#e6e3ef", line = "#3b2f6b"),
  output  = list(fill = "#ffffff", line = "#b03a2e"),
  config  = list(fill = "#faf8f5", line = "#8a8781"),
  edge    = "#5a5750",
  note    = "#6f6b64"
)

`%||%` <- function(a, b) if (is.null(a)) b else a


# NODES
# ------------------------------------------------------------------------------
# x, y are box centres; w, h are full width and height.

nodes <- tibble::tribble(
  ~id,        ~x,   ~y,  ~w,  ~h, ~grp,      ~label,                ~sub,
  # config spine
  "cfg",       6,   34,  10,  62, "config",  "00_config.R",         "all parameters\nno side effects",
  
  # inputs
  "osm",      23,   62,  18,   7, "input",   "OSM extracts",        ".pbf, 4 states",
  "gtfs",     45,   62,  18,   7, "input",   "GTFS feeds",          "5 agencies",
  "fac",      67,   62,  18,   7, "input",   "Facilities",          "n = 5",
  "org",      89,   62,  18,   7, "input",   "Synthetic origins",   "n = 5,000",
  
  # network
  "net",      56,   49,  84,   8, "network", "01_network.R",
  "merge + clip  \u00b7  prefix route IDs by operator  \u00b7  validate service on analysis date  \u00b7  build_network()",
  
  # routing
  "pop",      34,   33,  40,  10, "route",   "Population pass",
  "5,000 origins \u00d7 5 facilities \u00d7 8 arrival hours\nbreakdown = TRUE",
  "tmp",      78,   33,  40,  10, "route",   "Temporal pass",
  "1,000 origins \u00d7 5 facilities \u00d7 240 instants\n2-minute arrival grid",
  
  # aggregation
  "agg",      56,   17,  84,   8, "analyse", "04_aggregate.R",
  "complete origin \u00d7 facility \u00d7 time grid; unreachable pairs preserved as a result",
  
  # outputs
  "out",      56,    5,  84,   7, "output",  "05_figures_tables.R",
  "Tables 1a\u20131b  \u00b7  origin travel-time range  \u00b7  Maps A and C"
) |>
  mutate(fill = vapply(grp, function(g) pal[[g]]$fill, character(1)),
         line = vapply(grp, function(g) pal[[g]]$line, character(1)),
         xmin = x - w / 2, xmax = x + w / 2,
         ymin = y - h / 2, ymax = y + h / 2)


# EDGES
# ------------------------------------------------------------------------------
# Endpoints derived from node geometry, so edges track any layout change.

box <- function(id) nodes[nodes$id == id, ]

edge_v <- function(from, to, x_from = NULL, x_to = NULL) {
  a <- box(from); b <- box(to)
  data.frame(x = x_from %||% a$x, y = a$ymin,
             xend = x_to %||% b$x, yend = b$ymax)
}

edge_h <- function(from, to) {
  a <- box(from); b <- box(to)
  data.frame(x = a$xmax, y = a$y, xend = b$xmin, yend = b$y)
}

edges <- bind_rows(
  # inputs into the network build
  edge_v("osm",  "net"), edge_v("gtfs", "net"),
  edge_v("fac",  "net"), edge_v("org",  "net"),
  # network splits into the two routing passes
  edge_v("net", "pop", x_from = box("pop")$x),
  edge_v("net", "tmp", x_from = box("tmp")$x),
  # passes converge on aggregation
  edge_v("pop", "agg", x_to = box("pop")$x),
  edge_v("tmp", "agg", x_to = box("tmp")$x),
  edge_v("agg", "out")
)

# config feeds every stage
cfg_edges <- bind_rows(lapply(c("net", "pop", "agg", "out"), function(id) {
  b <- box(id)
  data.frame(x = box("cfg")$xmax, y = b$y, xend = b$xmin, yend = b$y)
}))

# what moves along each edge
notes <- tibble::tribble(
  ~x,  ~y,   ~label,
  58,  42.5, "network.dat + manifest",
  58,  25.0, "one RDS per call, resumable",
  58,  10.5, "analysis tables"
)


# PLOT
# ------------------------------------------------------------------------------

arrow_style <- arrow(length = unit(2, "mm"), type = "closed")

fig1 <- ggplot() +
  # config feeds, drawn first and dashed so they read as context
  geom_segment(data = cfg_edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = pal$edge, linewidth = 0.3, linetype = "22",
               arrow = arrow_style) +
  geom_segment(data = edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = pal$edge, linewidth = 0.4, arrow = arrow_style) +
  geom_rect(data = nodes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = nodes$fill, colour = nodes$line, linewidth = 0.5) +
  # label sits above centre, sub below, so multi-line subs stay inside the box
  geom_text(data = nodes, aes(x = x, y = y + h * 0.17, label = label),
            fontface = "bold", size = 3.1, colour = "grey12") +
  geom_text(data = nodes, aes(x = x, y = y - h * 0.20, label = sub),
            size = 2.4, colour = pal$note, lineheight = 1.05) +
  geom_text(data = notes, aes(x = x, y = y, label = label),
            size = 2.3, colour = pal$note, hjust = 0, fontface = "italic") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70), expand = FALSE) +
  theme_void()

ggsave(file.path(out_dir, "figure1_pipeline.png"), fig1,
       width = 9, height = 6.3, dpi = 400, bg = "white")
ggsave(file.path(out_dir, "figure1_pipeline.pdf"), fig1,
       width = 9, height = 6.3)

message("wrote figure1_pipeline.png and .pdf to ", out_dir)