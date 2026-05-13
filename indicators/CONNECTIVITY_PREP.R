# =============================================================================
# CONNECTIVITY_PREP.R — Spatial preprocessing for connectivity indicator
# =============================================================================
# Downloads OSM road network and market points, builds a friction surface,
# computes the cost-distance (travel time) to the nearest market, and
# extracts a per-hex-cell accessibility score.
#
# CHECKPOINTS: each major output is skipped if the file already exists.
# Run once for the full study area. A 70 km buffer ensures the OSM download
# covers road access beyond the study boundary.
#
# NOTE: If the friction / time-access rasters were previously computed for
# Ward 28 only, delete them so they are rebuilt for the full study area.
#
# Requires:
#   - 00_config.R already sourced (FILE_PATHS defined)
#   - Step 1 completed (lulc_reclass raster exists)
#   - data/processed/manual_market_points_buffered.shp  ← created in QGIS
#
# Output:
#   data/processed/connectivity_accessibility_by_cell.csv
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(osmdata)
  library(exactextractr)
})

# --- Parameters ---------------------------------------------------------------

osm_buffer_km     <- 70
road_speeds       <- c(
  trunk        = 100,
  primary      = 100,
  secondary    =  80,
  tertiary     =  30,
  unclassified =  10,
  service      =  10,
  track        =  10,
  residential  =  10
)
res_friction_m    <- 50     # spatial resolution of friction/cost rasters (m)
walking_speed_kmh <-  5     # off-road speed assumption (km/h)

# --- Base data ----------------------------------------------------------------

cat("  [CONN-PREP] Loading base data...\n")

study_area <- st_read(FILE_PATHS$wards,      quiet = TRUE)
lulc       <- rast(FILE_PATHS$lulc_reclass)
grid       <- st_read(FILE_PATHS$hex_grid_v4, quiet = TRUE)

crs_project      <- crs(lulc)
study_area_proj  <- st_transform(study_area, crs_project)
study_union_proj <- st_union(study_area_proj)   # single polygon for cropping

# 70 km buffer for OSM bbox (computed in Web Mercator for metric precision)
study_buf_wm   <- st_buffer(st_transform(study_union_proj, 3857),
                             dist = osm_buffer_km * 1000)
bbox_osm       <- st_bbox(st_transform(study_buf_wm, 4326))
study_buf_proj <- st_transform(study_buf_wm, crs_project)

# Hex grid — use the 'id' column as the canonical scenario identifier
grid_proj             <- st_transform(grid, crs_project)
grid_proj$scenario_id <- as.character(grid_proj$id)

cat("  [CONN-PREP] Wards:", nrow(study_area),
    "| Hex cells:", nrow(grid_proj),
    "| OSM bbox:", paste(round(as.numeric(bbox_osm), 3), collapse = " "), "\n")

# =============================================================================
# CHECKPOINT 1 — OSM market points
# =============================================================================

if (!file.exists(FILE_PATHS$market_points_osm)) {
  cat("  [CONN-PREP] [1/6] Downloading market points from OSM...\n")

  get_market_points <- function(bbox) {
    pts <- list()

    q_mp <- opq(bbox) %>%
      add_osm_feature(key = "amenity", value = "marketplace") %>%
      osmdata_sf()
    if (!is.null(q_mp$osm_points)   && nrow(q_mp$osm_points)   > 0)
      pts[["mp_pts"]]  <- q_mp$osm_points   %>% dplyr::select(geometry)
    if (!is.null(q_mp$osm_polygons) && nrow(q_mp$osm_polygons) > 0)
      pts[["mp_poly"]] <- st_centroid(q_mp$osm_polygons) %>% dplyr::select(geometry)

    q_tc <- opq(bbox) %>%
      add_osm_feature(key = "place", value = c("town", "city")) %>%
      osmdata_sf()
    if (!is.null(q_tc$osm_points) && nrow(q_tc$osm_points) > 0)
      pts[["towns"]] <- q_tc$osm_points %>% dplyr::select(geometry)

    if (length(pts) == 0) {
      warning("[CONN-PREP] No markets/towns found — falling back to villages.")
      q_vill <- opq(bbox) %>%
        add_osm_feature(key = "place", value = c("town", "city", "village")) %>%
        osmdata_sf()
      if (!is.null(q_vill$osm_points) && nrow(q_vill$osm_points) > 0)
        pts[["fallback"]] <- q_vill$osm_points %>% dplyr::select(geometry)
    }

    bind_rows(pts)
  }

  markets_osm  <- get_market_points(bbox_osm)
  markets_proj <- st_transform(markets_osm, crs_project)

  st_write(markets_proj, FILE_PATHS$market_points_osm, delete_layer = TRUE, quiet = TRUE)
  cat("  [CONN-PREP] OSM market points saved:", nrow(markets_proj), "features.\n")

} else {
  cat("  [CONN-PREP] [1/6] OSM market points found — skipping download.\n")
}

# =============================================================================
# CHECKPOINT 2 — Manual market points (must exist — edited in QGIS)
# =============================================================================
# Before this step, verify that all important markets / towns in the study area
# are present. If any are missing from OSM:
#   1. Open data/processed/market_points_osm.shp in QGIS
#   2. Add missing points manually
#   3. Save as data/processed/manual_market_points_buffered.shp
#   4. Re-run RUN_MAIN.R

cat("  [CONN-PREP] [2/6] Checking manual market points...\n")

if (!file.exists(FILE_PATHS$market_points_manual)) {
  stop(
    "[CONN-PREP] Manual market points file not found:\n  ",
    FILE_PATHS$market_points_manual, "\n",
    "Create it in QGIS from the OSM points and save, then re-run."
  )
}

markets_final <- st_read(FILE_PATHS$market_points_manual, quiet = TRUE) %>%
  st_transform(crs_project) %>%
  dplyr::select(geometry)

cat("  [CONN-PREP] Manual market points loaded:", nrow(markets_final), "\n")

# =============================================================================
# CHECKPOINT 3 — OSM road network
# =============================================================================

if (!file.exists(FILE_PATHS$roads_osm)) {
  cat("  [CONN-PREP] [3/6] Downloading road network from OSM...\n")

  roads_raw  <- opq(bbox_osm) %>%
    add_osm_feature(key = "highway", value = names(road_speeds)) %>%
    osmdata_sf()

  roads_sf <- roads_raw$osm_lines %>%
    dplyr::select(highway) %>%
    filter(highway %in% names(road_speeds))

  roads_proj           <- st_transform(roads_sf, crs_project)
  roads_proj$speed_kmh <- road_speeds[roads_proj$highway]

  st_write(roads_proj, FILE_PATHS$roads_osm, delete_layer = TRUE, quiet = TRUE)
  cat("  [CONN-PREP] Road segments saved:", nrow(roads_proj), "\n")
  print(table(roads_proj$highway))

} else {
  cat("  [CONN-PREP] [3/6] Road network found — skipping download.\n")
  roads_proj <- st_read(FILE_PATHS$roads_osm, quiet = TRUE) %>%
    st_transform(crs_project)
}

# =============================================================================
# CHECKPOINT 4 — Friction raster
# =============================================================================
# Friction = inverse speed (hours per metre):
#   on road  → 1 / (speed_kmh × 1000)
#   off road → 1 / (walking_speed_kmh × 1000)

if (!file.exists(FILE_PATHS$friction_raster)) {
  cat("  [CONN-PREP] [4/6] Building friction raster (", res_friction_m, "m)...\n")

  rast_template <- rast(
    ext(vect(study_buf_proj)),
    resolution = res_friction_m,
    crs        = crs_project
  )

  roads_speed_rast <- rasterize(vect(roads_proj), rast_template, field = "speed_kmh")

  friction <- ifel(
    is.na(roads_speed_rast),
    1 / (walking_speed_kmh * 1000),
    1 / (roads_speed_rast * 1000)
  )
  names(friction) <- "friction"

  writeRaster(friction, FILE_PATHS$friction_raster, overwrite = TRUE)
  cat("  [CONN-PREP] Friction raster saved.\n")

} else {
  cat("  [CONN-PREP] [4/6] Friction raster found — skipping creation.\n")
  friction <- rast(FILE_PATHS$friction_raster)
}

# =============================================================================
# CHECKPOINT 5 — Cost-distance surface (travel time to nearest market)
# =============================================================================

if (!file.exists(FILE_PATHS$time_access_raster)) {
  cat("  [CONN-PREP] [5/6] Computing cost-distance surface...\n")

  # Set friction = 0 at market locations so costDist treats them as sources
  markets_rast <- rasterize(vect(markets_final), friction, fun = "count")
  markets_rast <- ifel(markets_rast > 0, 1, NA)

  friction_src <- friction
  friction_src[markets_rast == 1] <- 0

  time_access_r        <- terra::costDist(friction_src, target = 0)
  names(time_access_r) <- "time_hours"

  writeRaster(time_access_r, FILE_PATHS$time_access_raster, overwrite = TRUE)
  time_vals <- values(time_access_r, na.rm = TRUE)
  cat("  [CONN-PREP] Time-access raster saved.",
      "Range (h):", round(min(time_vals), 2), "—", round(max(time_vals), 2), "\n")

} else {
  cat("  [CONN-PREP] [5/6] Time-access raster found — skipping computation.\n")
  time_access_r <- rast(FILE_PATHS$time_access_raster)
}

# =============================================================================
# CHECKPOINT 6 — Per-hex-cell accessibility extraction
# =============================================================================

if (!file.exists(FILE_PATHS$connectivity_access)) {
  cat("  [CONN-PREP] [6/6] Extracting accessibility scores per hex cell...\n")

  # Crop LULC to study area (speeds up extraction)
  lulc_study <- crop(lulc, vect(study_union_proj)) %>% mask(vect(study_union_proj))
  names(lulc_study) <- "lulc"

  # Align time-access surface to LULC resolution/extent
  time_resampled <- resample(time_access_r, lulc_study, method = "bilinear")
  stack_r        <- c(lulc_study, time_resampled)

  # Pixel-level extraction per hex cell (coverage_fraction for area weighting)
  pixels_raw <- exact_extract(
    stack_r,
    grid_proj,
    include_cols = "scenario_id",
    include_xy   = FALSE
  )
  pixels_df <- bind_rows(pixels_raw) %>%
    filter(!is.na(lulc), !is.na(time_hours))

  cat("  [CONN-PREP] Pixels extracted:", nrow(pixels_df), "\n")

  # Median travel time per cell × LULC class (robust to outlier pixels)
  time_by_lulc <- pixels_df %>%
    group_by(scenario_id, lulc) %>%
    summarise(
      median_time = median(time_hours, na.rm = TRUE),
      area_px     = sum(coverage_fraction, na.rm = TRUE),
      .groups = "drop"
    )

  # Cell-level score = area-weighted mean of per-class medians
  accessibility_by_cell <- time_by_lulc %>%
    group_by(scenario_id) %>%
    summarise(
      accessibility_score_brut = weighted.mean(median_time, area_px, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(accessibility_by_cell, FILE_PATHS$connectivity_access, row.names = FALSE)
  cat("  [CONN-PREP] Accessibility scores saved for",
      nrow(accessibility_by_cell), "cells.\n")

} else {
  cat("  [CONN-PREP] [6/6] Accessibility CSV found — skipping extraction.\n")
}

cat("  [CONN-PREP] Preprocessing complete.\n")
