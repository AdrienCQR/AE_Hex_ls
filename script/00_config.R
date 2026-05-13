# =============================================================================
# 00_config.R — Parameters and file paths
# =============================================================================
# Edit this file to change any parameter before running RUN_MAIN.R.
# All other scripts read their settings from here.
# =============================================================================

VERSION <- "V1.0"

# --- File paths ---------------------------------------------------------------

FILE_PATHS <- list(
  # Raw inputs (all inside data/ so the folder is self-contained)
  lulc_raw        = "data/raw/lulc/V2_land_use_map_garden_32736.tif",
  lulc_codes      = "data/raw/lulc/V2_land_use_map_codes.csv",
  wards           = "data/raw/communal_wards/communal_wards.shp",
  baseline        = "data/processed/baseline_compo.csv",

  # Intermediate outputs (hex database, step 1)
  lulc_reclass    = "data/lulc_processed/V2_lulc_reclassified.tif",
  lulc_reclass_csv = "data/lulc_processed/V2_lulc_reclassified.csv",
  hex_grid_v4     = "data/grid_tool/V4_grid_hex_5km2_filtered_final.shp",
  hex_compositions = "data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv",

  # Final results (step 2)
  output_results  = "results/v3_final_results_with_composite_scores.csv",

  # Map outputs folder (steps 3-4)
  maps_dir        = "results/maps/"
)

# --- Indicators to run --------------------------------------------------------
# Set to TRUE to include, FALSE to skip.

INDICATORS_TO_RUN <- list(
  SOIL_HEALTH            = TRUE,
  BIODIV                 = TRUE,
  WOOD                   = TRUE,
  N_LITTER_TREES         = TRUE,
  N_LEGUMES_ROTATION     = TRUE,
  GRAZING_CC             = TRUE,
  AGRO_PASTO_LOOP        = TRUE,
  ECO_DIV                = TRUE,
  FAIRNESS               = TRUE,
  FSS                    = FALSE,
  FOOD_ECONOMIC_SECURITY = FALSE,
  FOOD_SECURITY          = FALSE,
  VULNERABILITY          = FALSE,
  NUTRITION_DIVERSITY    = FALSE,
  CONNECTIVITY_VAL_CHAIN = TRUE,
  SYNERGY                = TRUE,
  AE_COMPOSITE           = TRUE,
  FSN_COMPOSITE          = FALSE
)

# --- Landscape parameters -----------------------------------------------------

TOTAL_AREA_HA <- 500          # Hex cell area in hectares

R             <- 832          # Annual rainfall (mm) — Murehwa average
wet_months    <- 6            # Number of wet months (Nov–May)
winter_months <- 12 - wet_months

# Crop shares in cropland (must sum to 1)
maize_share   <- 0.74         # Manyanga et al., 2025
legume_share  <- 0.21
tobacco_share <- 0.05

# Tree cover fraction per land use class (baseline)
tree_hedge_cropland  <- 0.059
tree_hedge_grassland <- 0.034

# --- Population parameters ----------------------------------------------------

pop_density  <- 71            # Inhabitants / km2 (rural communal wards)
total_pop    <- pop_density * (TOTAL_AREA_HA / 100)
farming_pop  <- total_pop * 0.70
HH_size      <- 5
HH_number    <- round(total_pop / HH_size, 0)
HH_farming   <- round(farming_pop / HH_size, 0)
HH_wood_demand <- 1.7         # Tons of wood per household per year

# --- Nitrogen parameters ------------------------------------------------------

NDFA_rate                <- 0.62   # N from atmosphere (groundnuts)
HI_gdnuts                <- 0.40   # Harvest index (groundnuts)
collectable_litter_per_HH <- 1.25  # Tons / HH / year

# --- Grazing parameters -------------------------------------------------------

RUE                  <- 2.35   # Rain use efficiency (Rufino 2011)
cattle_weight        <- 250    # kg per head
cattle_daily_intake  <- 0.03   # Fraction of body weight / day
cattle_security_threshold <- 4.5  # cattle / farming HH

# --- Agro-pastoral loop -------------------------------------------------------

tolerance               <- 0.5
max_iterations          <- 30
residue_to_grain_ratio  <- 1.5
residue_degradation     <- 0.75
residue_feed_share      <- 1.0
damage_level            <- 0.70   # Fraction of yield remaining after losses

# --- Wood parameters ----------------------------------------------------------

woody_coefficient           <- 0.935
regeneration_years          <- 28.5   # Miombo full regeneration (Frost 1996)
wood_demand_per_ha_tobacco  <- 18.5   # Tons / ha of tobacco cured

# --- Nutrition and food security ----------------------------------------------

min_diversity_target                  <- 5
threshold_woodland_ha_per_capita      <- 0.2

# --- Composite indicator columns ----------------------------------------------
# These define which indicator outputs are included in the AE composite score.

AE_INDICATOR_COLS <- c(
  "final_N_total_per_ha",    # Recycling
  "total_N_legumes",         # Input use reduction
  "soc_density_ha",          # Soil health
  "biodiv_score",            # Biodiversity
  "eco_div_shannon_raw",     # Economic diversity
  "equity_raw",              # Fairness
  "connectivity_score_brut", # Value chain connectivity
  "synergy_score_sum"        # Synergy
)
