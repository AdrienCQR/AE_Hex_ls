# =============================================================================
# RUN_MAIN.R
# =============================================================================
# Central pipeline — AE Hexagonal Landscape Analysis (Murehwa, Zimbabwe)
#
# Run from the AE_Hex_ls project root in RStudio:
#   source("RUN_MAIN.R")
#
# Pipeline steps:
#   1. Build hex landscape database (skipped if output already exists)
#   2. Calculate all agroecological indicators (skipped if output exists)
#   3. Produce maps
#   4. Synergy / losses / trade-off analysis
#
# Edit script/00_config.R to change parameters or file paths.
# =============================================================================

rm(list = ls())

# --- 0. Load parameters -------------------------------------------------------

source("script/00_config.R")
cat("Parameters loaded — version:", VERSION, "\n\n")

# --- 1. Build hex landscape database ------------------------------------------
# Input:  data/raw/lulc raster + communal wards shapefile
# Output: lulc_compo_mur_ward_towm_filtered_v4.csv  +  V4 hex grid shapefile
# Checkpoint: skipped if the composition CSV already exists.

if (!file.exists(FILE_PATHS$hex_compositions)) {
  cat("[1/4] Building hex database...\n")
  source("script/01_build_hex_database.R")
  cat("[1/4] Done.\n\n")
} else {
  cat("[1/4] Hex database found — skipping creation.\n\n")
}

# --- 2. Calculate all agroecological indicators --------------------------------
# Input:  hex compositions CSV  +  baseline data
# Output: v3_final_results_with_composite_scores.csv
# Checkpoint: skipped if the results file already exists.

if (!file.exists(FILE_PATHS$output_results)) {
  cat("[2/4] Running indicators...\n")
  source("script/02_run_indicators.R")
  cat("[2/4] Done.\n\n")
} else {
  cat("[2/4] Results file found — skipping indicators.\n\n")
}

# --- 3. Produce maps ----------------------------------------------------------
# Input:  results CSV  +  V4 hex grid  +  ward boundaries
# Output: PNG maps in AE_Hex_ls/results/maps3/

cat("[3/4] Producing maps...\n")
source("script/03_maps.R")
cat("[3/4] Maps saved to AE_Hex_ls/results/maps3/\n\n")

# --- 4. Synergy / losses / trade-off analysis ---------------------------------
# Input:  results CSV  +  V4 hex grid
# Output: PNG figures in AE_Hex_ls/results/maps3/

cat("[4/4] Running synergy analysis...\n")
source("script/04_synergies.R")
cat("[4/4] Done.\n\n")

cat("=== Pipeline complete ===\n")
