# =============================================================================
# 01_build_hex_database.R
# =============================================================================
# Builds the hexagonal landscape database for Murehwa.
#
# Steps:
#   A. Reclassify raw LULC raster (merge mineral + bare soil into one class)
#   B. Create 5 km2 hexagonal grid and filter to valid LULC coverage + wards
#   C. Extract LULC composition per hex cell
#   D. Filter hexagons (cropland >= 10%, mineral_bare_soil <= 30%)
#   E. Save final hex grid (V4) and composition table (v4 CSV)
#
# Inputs:
#   data/raw/lulc/V2_land_use_map_garden_32736.tif
#   data/raw/lulc/V2_land_use_map_codes.csv
#   data/raw/communal_wards/communal_wards.shp
#
# Outputs:
#   AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif
#   AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv
#   AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp
#   AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv
# =============================================================================

# --- Packages ----------------------------------------------------------------
pkgs <- c("sf", "terra", "dplyr", "units", "exactextractr")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# =============================================================================
# A. Reclassify LULC raster
# =============================================================================

cat("  [A] Reclassifying LULC raster...\n")

lulc <- rast(FILE_PATHS$lulc_raw)
legend_raw <- read.csv2(FILE_PATHS$lulc_codes)

# Merge bare soil (class 5) into mineral bare soil (class 3)
reclass_matrix <- matrix(
  c(0, 0,   # dense_woodland
    1, 1,   # cropland
    2, 2,   # open_woodland
    3, 3,   # mineral
    4, 4,   # urban
    5, 3,   # bare_soil -> mineral
    6, 6,   # water
    7, 7,   # horticulture
    8, 8,   # grassland
    9, 9),  # wetland_grassland
  ncol = 2, byrow = TRUE
)

lulc_reclass <- classify(lulc, reclass_matrix)

legend_reclass <- data.frame(
  class_code = c(0, 1, 2, 3, 4, 6, 7, 8, 9),
  class_name = c(
    "dense_woodland", "cropland", "open_woodland", "mineral_bare_soil",
    "urban", "water", "horticulture", "grassland", "wetland_grassland"
  ),
  color = c(
    "#236832", "#ffc767", "#39a751", "#7f8183",
    "#fc8163", "#2550ff", "#9b59b6", "#d3ff6b", "#b0f0cf"
  )
)

writeRaster(lulc_reclass, FILE_PATHS$lulc_reclass, overwrite = TRUE)
write.csv2(legend_reclass, FILE_PATHS$lulc_reclass_csv, row.names = FALSE)
cat("  [A] LULC reclassified and saved.\n")

# =============================================================================
# B. Create hexagonal grid and filter
# =============================================================================

cat("  [B] Creating hexagonal grid...\n")

# Build 5 km2 hexagonal grid over the LULC extent
target_area_m2 <- 5 * 1e6
hex_side_m     <- sqrt(target_area_m2 / (3 * sqrt(3) / 2))
hex_cellsize   <- sqrt(3) * hex_side_m

bbox_sf  <- st_as_sfc(st_bbox(lulc_reclass))
hex_grid <- st_make_grid(bbox_sf, cellsize = hex_cellsize, square = FALSE)
hex_grid <- st_sf(id = seq_along(hex_grid), geometry = hex_grid)

# Keep only hexagons fully covered by LULC data
masque      <- ifel(!is.na(lulc_reclass), 1, NA)
masque_poly <- st_as_sf(as.polygons(masque, dissolve = TRUE))
hex_in_lulc <- as.logical(st_within(hex_grid, masque_poly, sparse = FALSE))
hex_lulc    <- hex_grid[hex_in_lulc, ]

# Keep only hexagons that intersect communal wards
ward <- st_read(FILE_PATHS$wards, quiet = TRUE)
ward$geometry <- st_make_valid(ward$geometry)
hex_in_ward  <- apply(st_intersects(hex_lulc, ward, sparse = FALSE), 1, any)
hex_v2       <- hex_lulc[hex_in_ward, ]

# Remove hexagons covering the urban area of Murehwa town
hex_v2 <- hex_v2[!hex_v2$id %in% c(441, 459, 460, 477), ]

cat("  [B]", nrow(hex_v2), "hexagons after filtering.\n")

# =============================================================================
# C. Extract LULC composition per hex cell
# =============================================================================

cat("  [C] Extracting LULC proportions per hex...\n")

extract_lulc_proportions <- function(lulc_r, hex, legend) {
  props <- data.frame(exact_extract(lulc_r, hex, "frac"))
  props$scenario_id <- as.character(hex$id)

  # Rename frac_X columns to LULC class names
  for (i in seq_len(nrow(legend))) {
    old <- paste0("frac_", legend$class_code[i])
    if (old %in% names(props)) {
      names(props)[names(props) == old] <- legend$class_name[i]
    }
  }

  # Ensure all expected classes exist (fill with 0 if absent)
  expected <- c(
    "scenario_id", "wetland_grassland", "horticulture", "cropland",
    "mineral_bare_soil", "dense_woodland", "open_woodland",
    "grassland", "urban", "water"
  )
  for (col in setdiff(expected, names(props))) props[[col]] <- 0
  props[is.na(props)] <- 0

  props[, expected]
}

df_compo <- extract_lulc_proportions(lulc_reclass, hex_v2, legend_reclass)

# =============================================================================
# D. Filter hexagons: cropland >= 10 %, mineral_bare_soil <= 30 %
# =============================================================================

cat("  [D] Filtering hexagons by land-use thresholds...\n")

keep <- df_compo$cropland >= 0.10 & df_compo$mineral_bare_soil <= 0.30
hex_v4       <- hex_v2[keep, ]
df_compo_v4  <- df_compo[keep, ]

cat("  [D]", nrow(hex_v4), "hexagons retained.\n")

# =============================================================================
# E. Save outputs
# =============================================================================

cat("  [E] Saving outputs...\n")

writeVector(
  vect(hex_v4),
  FILE_PATHS$hex_grid_v4,
  overwrite = TRUE
)

write.csv(df_compo_v4, FILE_PATHS$hex_compositions, row.names = FALSE)

cat("  [E] Hex database ready.\n")
