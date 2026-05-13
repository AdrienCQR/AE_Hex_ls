# =============================================================================
# 02_run_indicators.R
# =============================================================================
# Loads the hex landscape compositions, runs all agroecological indicators,
# builds the AE composite score, and saves the final results table.
#
# Requires: 00_config.R already sourced (done by RUN_MAIN.R)
#
# Inputs:
#   AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv
#   data/processed/baseline_compo.csv
#   R/run_pipeline_AE_HEX_ls.R  (indicator engine)
#
# Outputs:
#   AE_Hex_ls/results/v3_final_results_with_composite_scores.csv
# =============================================================================

# --- Packages ----------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "sf", "terra")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Load landscape compositions ----------------------------------------------

cat("  Loading landscape compositions...\n")

if (!file.exists(FILE_PATHS$hex_compositions)) {
  stop("Hex compositions not found. Run step 1 first (01_build_hex_database.R).")
}

scenarios_raw          <- read.csv(FILE_PATHS$hex_compositions)
scenarios_raw$scenario_id <- as.character(scenarios_raw$scenario_id)

baseline               <- read.csv2(FILE_PATHS$baseline)
scenarios_raw          <- bind_rows(baseline, scenarios_raw)

cat("  ", nrow(scenarios_raw), "landscape scenarios loaded.\n")

# --- Run all indicators -------------------------------------------------------

source("indicators/run_pipeline.R")

res <- run_all_indicators(
  scenarios_raw      = scenarios_raw,
  INDICATORS_TO_RUN  = INDICATORS_TO_RUN,
  params = list(
    TOTAL_AREA_HA              = TOTAL_AREA_HA,
    tree_hedge_cropland        = tree_hedge_cropland,
    tree_hedge_grassland       = tree_hedge_grassland,
    HH_number                  = HH_number,
    maize_share                = maize_share,
    collectable_litter_per_HH  = collectable_litter_per_HH,
    NDFA_rate                  = NDFA_rate,
    HI_gdnuts                  = HI_gdnuts,
    legume_share               = legume_share,
    RUE                        = RUE,
    R                          = R,
    wet_months                 = wet_months,
    cattle_daily_intake        = cattle_daily_intake,
    cattle_weight              = cattle_weight,
    tolerance                  = tolerance,
    max_iterations             = max_iterations,
    residue_to_grain_ratio     = residue_to_grain_ratio,
    residue_degradation        = residue_degradation,
    residue_feed_share         = residue_feed_share,
    winter_months              = winter_months,
    damage_level               = damage_level,
    tobacco_share              = tobacco_share,
    total_pop                  = total_pop,
    HH_wood_demand             = HH_wood_demand,
    HH_farming                 = HH_farming,
    min_diversity_target       = min_diversity_target,
    threshold_woodland_ha_per_capita = threshold_woodland_ha_per_capita,
    woody_coefficient          = woody_coefficient,
    regeneration_years         = regeneration_years,
    wood_demand_per_ha_tobacco = wood_demand_per_ha_tobacco
  ),
  synergy_opts = list(
    palette   = "viridis::magma",
    direction = -1,
    style     = "quantile",
    K         = 4
  ),
  print_synergy_plot = FALSE
)

indicators_results <- res$indicators_results

# --- Build AE composite score -------------------------------------------------

normalize_min_max <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

composite_indicators <- indicators_results %>%
  select(scenario_id, all_of(AE_INDICATOR_COLS)) %>%
  mutate(across(all_of(AE_INDICATOR_COLS), normalize_min_max, .names = "{.col}_norm")) %>%
  rowwise() %>%
  mutate(
    # Geometric mean with small offset to avoid log(0)
    ae_composite_score = exp(mean(log(c_across(ends_with("_norm")) + 0.01)))
  ) %>%
  ungroup() %>%
  rename(
    RECYCLING_norm     = final_N_total_per_ha_norm,
    INPUT_norm         = total_N_legumes_norm,
    SOIL_HEALTH_norm   = soc_density_ha_norm,
    BIODIV_norm        = biodiv_score_norm,
    ECONOMIC_DIV_norm  = eco_div_shannon_raw_norm,
    FAIRNESS_norm      = equity_raw_norm,
    CONNECT_norm       = connectivity_score_brut_norm,
    SYNERGY_norm       = synergy_score_sum_norm
  ) %>%
  select(scenario_id, ae_composite_score, ends_with("_norm"))

# --- Merge and save -----------------------------------------------------------

indicators_results_final <- indicators_results %>%
  left_join(composite_indicators, by = "scenario_id")

write_csv(indicators_results_final, FILE_PATHS$output_results)

cat("  Results saved to", FILE_PATHS$output_results, "\n")
cat("  Composite score — min:", round(min(indicators_results_final$ae_composite_score, na.rm = TRUE), 3),
    "/ max:", round(max(indicators_results_final$ae_composite_score, na.rm = TRUE), 3), "\n")
