# =============================================================================
# run_pipeline.R
# =============================================================================
# Engine that calls each individual indicator in sequence.
# All indicator scripts are resolved relative to AE_Hex_ls/indicators/.
# Call via: source("AE_Hex_ls/indicators/run_pipeline.R")
#           res <- run_all_indicators(...)
# =============================================================================

run_all_indicators <- function(
  scenarios_raw,
  INDICATORS_TO_RUN,
  params,
  source_files = list(
    tree_hedges   = "indicators/tree_hedges_processing.R",
    soil          = "indicators/SOIL_HEALTH.R",
    biodiv        = "indicators/BIODIV.R",
    n_litter      = "indicators/N_LITTER_TREES.R",
    n_legumes     = "indicators/N_LEGUMES_ROTATION.R",
    grazing       = "indicators/GRAZING_CC.R",
    agro_pasto    = "indicators/AGRO_PASTO_LOOP.R",
    eco_div       = "indicators/ECO_DIV.R",
    fairness      = "indicators/FAIRNESS.R",
    wood          = "indicators/WOOD.R",
    connectivity  = "indicators/CONNECTIVITY_VAL_CHAIN.R",
    synergy       = "indicators/SYNERGY.R"
  ),
  synergy_opts       = list(),
  print_synergy_plot = TRUE,
  stop_on_error      = TRUE,
  verbose            = TRUE
) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(ggplot2)
  })

  msg  <- function(...) if (isTRUE(verbose)) cat(...)
  need <- function(name) {
    if (!name %in% names(params)) stop("Missing parameter: '", name, "'")
    params[[name]]
  }
  timed <- function(label, code) {
    t <- system.time(
      res <- if (isTRUE(stop_on_error)) force(code) else try(force(code), silent = TRUE)
    )
    msg(sprintf("   [%.2fs] %s\n", t["elapsed"], label))
    if (inherits(res, "try-error")) stop(sprintf("Step '%s' failed:\n%s", label, as.character(res)))
    res
  }

  # --- Pre-processing: extract tree hedges from cropland / grassland ----------
  msg("Pre-processing: extracting tree hedges...\n")
  source(source_files$tree_hedges)
  scenarios_processed <- timed("tree_hedges",
    extract_tree_hedges(scenarios_raw,
                        need("tree_hedge_cropland"),
                        need("tree_hedge_grassland"))
  )
  scenarios_raw <- scenarios_processed

  # Long format with area in hectares
  scenarios_long <- scenarios_raw %>%
    pivot_longer(cols = -scenario_id, names_to = "LULC_Class", values_to = "proportion") %>%
    mutate(area_ha = proportion * need("TOTAL_AREA_HA")) %>%
    filter(LULC_Class != "total_check")

  msg(nrow(scenarios_raw), "landscape scenarios loaded.\n")

  # --- Initialise results table -----------------------------------------------
  indicators_results <- scenarios_raw %>%
    mutate(across(-scenario_id, ~ .x * need("TOTAL_AREA_HA"), .names = "{.col}_ha")) %>%
    relocate(scenario_id) %>%
    select(scenario_id, matches("^(?!.*_ha$).*$", perl = TRUE), ends_with("_ha"))

  # --- Indicators -------------------------------------------------------------
  msg("\nRunning indicators:\n")

  if (isTRUE(INDICATORS_TO_RUN$SOIL_HEALTH)) {
    msg("  SOIL_HEALTH\n"); source(source_files$soil)
    indicators_results <- timed("SOIL_HEALTH",
      calculate_soc_indicator(scenarios_long, indicators_results))
  }

  if (isTRUE(INDICATORS_TO_RUN$BIODIV)) {
    msg("  BIODIV\n"); source(source_files$biodiv)
    indicators_results <- timed("BIODIV",
      calculate_biodiv_indicator(scenarios_long, indicators_results))
  }

  if (isTRUE(INDICATORS_TO_RUN$N_LITTER_TREES)) {
    msg("  N_LITTER_TREES\n"); source(source_files$n_litter)
    indicators_results <- timed("N_LITTER_TREES",
      calculate_N_litter(scenarios_long, indicators_results,
                         need("HH_number"), need("maize_share"),
                         need("collectable_litter_per_HH")))
  }

  if (isTRUE(INDICATORS_TO_RUN$N_LEGUMES_ROTATION)) {
    msg("  N_LEGUMES_ROTATION\n"); source(source_files$n_legumes)
    indicators_results <- timed("N_LEGUMES_ROTATION",
      calculate_N_fixed(indicators_results, need("NDFA_rate"),
                        need("HI_gdnuts"), need("legume_share")))
  }

  if (isTRUE(INDICATORS_TO_RUN$GRAZING_CC)) {
    msg("  GRAZING_CC\n"); source(source_files$grazing)
    indicators_results <- timed("GRAZING_CC",
      calculate_grazing_capacity(scenarios_long, indicators_results,
                                 need("RUE"), need("R"), need("wet_months"),
                                 need("cattle_daily_intake"), need("cattle_weight"),
                                 need("TOTAL_AREA_HA")))
  }

  if (isTRUE(INDICATORS_TO_RUN$AGRO_PASTO_LOOP)) {
    msg("  AGRO_PASTO_LOOP\n"); source(source_files$agro_pasto)
    res_ap <- timed("AGRO_PASTO_LOOP",
      agro_pasto_loop(indicators_results,
                      need("tolerance"), need("max_iterations"),
                      need("residue_to_grain_ratio"), need("residue_degradation"),
                      need("residue_feed_share"), need("winter_months"),
                      need("cattle_daily_intake"), need("cattle_weight"),
                      need("maize_share"), need("damage_level")))
    indicators_results  <- res_ap$indicators_results
    convergence_history <- res_ap$convergence_history
    results_complete    <- res_ap$results_complete
  }

  if (isTRUE(INDICATORS_TO_RUN$ECO_DIV)) {
    msg("  ECO_DIV\n"); source(source_files$eco_div)
    indicators_results <- timed("ECO_DIV",
      calculate_economic_diversification(scenarios_raw, indicators_results,
                                         need("maize_share"), need("legume_share"),
                                         need("tobacco_share")))
  }

  if (isTRUE(INDICATORS_TO_RUN$FAIRNESS)) {
    msg("  FAIRNESS\n"); source(source_files$fairness)
    indicators_results <- timed("FAIRNESS",
      calculate_fairness_weighted(scenarios_raw, indicators_results))
  }

  if (isTRUE(INDICATORS_TO_RUN$WOOD)) {
    msg("  WOOD\n"); source(source_files$wood)
    indicators_results <- timed("WOOD",
      calculate_wood_indicators(scenarios_long, need("woody_coefficient"),
                                need("regeneration_years"), need("HH_number"),
                                need("HH_wood_demand"), indicators_results,
                                need("wood_demand_per_ha_tobacco"), need("tobacco_share"),
                                need("TOTAL_AREA_HA")))
  }

  if (isTRUE(INDICATORS_TO_RUN$CONNECTIVITY_VAL_CHAIN)) {
    msg("  CONNECTIVITY_VAL_CHAIN\n"); source(source_files$connectivity)
    indicators_results <- timed("CONNECTIVITY_VAL_CHAIN",
      calculate_value_chain_connectivity(indicators_results,
                                         need("maize_share"), need("legume_share"),
                                         need("tobacco_share")))
  }

  # --- Synergy score ----------------------------------------------------------
  plots <- list()
  if (isTRUE(INDICATORS_TO_RUN$SYNERGY)) {
    msg("  SYNERGY\n"); source(source_files$synergy)

    default_synergy_opts <- list(
      K            = 5,
      style        = "equal",
      fixed_breaks = NULL,
      rename_map   = c(
        RECYCLING    = "final_N_total_per_ha",
        INPUT        = "total_N_legumes",
        SOIL_HEALTH  = "soc_stocks",
        BIODIV       = "biodiv_score",
        ECONOMIC_DIV = "economic_diversification_score",
        FAIRNESS     = "equity_score",
        CONNECT      = "connectivity_score_brut"
      ),
      higher_better = c(
        RECYCLING = TRUE, INPUT = TRUE, SOIL_HEALTH = TRUE, BIODIV = TRUE,
        ECONOMIC_DIV = TRUE, FAIRNESS = TRUE, CONNECT = TRUE
      ),
      palette   = "RdYlGn",
      direction = -1
    )

    final_synergy_opts <- utils::modifyList(default_synergy_opts, synergy_opts)

    res_syn <- timed("SYNERGY",
      do.call(calculate_synergy_scores,
              c(list(indicators_results = indicators_results), final_synergy_opts)))

    indicators_results <- res_syn$indicators_results
    plots$synergy      <- res_syn$p_classes

    if (isTRUE(print_synergy_plot)) print(plots$synergy)
    msg("  Total columns computed:", ncol(indicators_results) - 1, "\n")
  }

  list(
    scenarios_processed = scenarios_raw,
    scenarios_long      = scenarios_long,
    indicators_results  = indicators_results,
    plots               = plots
  )
}
