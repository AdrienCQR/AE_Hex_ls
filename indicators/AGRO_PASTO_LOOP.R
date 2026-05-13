# Agro-pastoral loop: iterative convergence between livestock numbers and feed availability.
# Livestock numbers are adjusted until TLU from grazing capacity equals TLU from residue supply.
# Yield response curves fitted to experimental data from sandy soils (Murehwa context).
agro_pasto_loop <- function(
  indicators_results,
  tolerance,
  max_iterations,
  residue_to_grain_ratio,
  residue_degradation,
  residue_feed_share,
  winter_months,
  cattle_daily_intake,
  cattle_weight,
  maize_share,
  damage_level
) {
  # Manure parameters (Falconnier et al., 2023; Rusinamhodzi 2015)
  collection_rate       <- 0.4    # fraction of manure that is collectable
  manure_t_per_TLU      <- 1.022  # t manure / TLU / year
  N_content_manure      <- 0.014
  P_content_manure      <- 0.002

  # Winter feed need per TLU
  daily_need_kg             <- cattle_daily_intake * cattle_weight
  days_winter               <- (365 / 12) * winter_months
  winter_feed_per_TLU_tons  <- (daily_need_kg * days_winter) / 1000

  # Maize yield response models (polynomial, fitted to experimental data)
  data_sandy_p_ge_30 <- data.frame(x = c(0, 30, 60, 90, 120), y = c(1.19, 2.7, 2.97, 3.24, 3.73))
  data_sandy_p_lt_30 <- data.frame(x = c(0, 30, 60, 90, 120), y = c(0.645, 1.79, 1.823, 2.193, 2.016))
  model_sandy_p_ge_30 <- lm(y ~ poly(x, 2), data = data_sandy_p_ge_30)
  model_sandy_p_lt_30 <- lm(y ~ poly(x, 2), data = data_sandy_p_lt_30)

  clamp <- function(x, lo, hi) pmax(pmin(x, hi), lo)

  predict_yield <- function(N_kg_ha, P_kg_ha) {
    N_c <- clamp(N_kg_ha, 0, 120)
    if (P_kg_ha >= 30) predict(model_sandy_p_ge_30, data.frame(x = N_c))
    else               predict(model_sandy_p_lt_30, data.frame(x = N_c))
  }

  # Initial state
  n                    <- nrow(indicators_results)
  maize_ha_total       <- indicators_results$maize_ha
  N_litter_per_ha      <- indicators_results$N_litter_kg_per_maize_ha
  N_legumes_per_ha     <- indicators_results$N_legumes_fixed
  P_litter_per_ha      <- indicators_results$P_litter_kg_per_maize_ha
  CC_grazing           <- indicators_results$max_grazing_CC_TLU

  current_TLU          <- CC_grazing
  converged            <- rep(FALSE, n)
  convergence_iteration <- rep(NA, n)
  convergence_history  <- matrix(NA, nrow = n, ncol = max_iterations)
  actual_iterations    <- max_iterations

  for (iter in seq_len(max_iterations)) {
    N_manure_per_ha <- (current_TLU * manure_t_per_TLU * collection_rate * N_content_manure * 1000) / maize_ha_total
    P_manure_per_ha <- (current_TLU * manure_t_per_TLU * collection_rate * P_content_manure * 1000) / maize_ha_total

    N_total_per_ha <- N_manure_per_ha + N_litter_per_ha + N_legumes_per_ha
    P_total_per_ha <- P_manure_per_ha + P_litter_per_ha

    sandy_yield           <- mapply(predict_yield, N_total_per_ha, P_total_per_ha)
    total_maize_prod      <- maize_ha_total * sandy_yield * damage_level
    total_residues        <- total_maize_prod * residue_to_grain_ratio * residue_degradation * residue_feed_share

    winter_grass_tons     <- (indicators_results$total_grass_biomass * 0.33) / 1000
    total_winter_feed     <- winter_grass_tons + total_residues
    CC_residues           <- total_winter_feed / winter_feed_per_TLU_tons

    new_TLU <- pmin(CC_grazing, CC_residues)
    convergence_history[, iter] <- new_TLU

    if (iter > 1) {
      newly_converged <- !converged & (abs(new_TLU - current_TLU) < tolerance)
      convergence_iteration[newly_converged] <- iter
      converged <- converged | newly_converged
    }

    current_TLU <- new_TLU

    if (all(converged)) {
      actual_iterations <- iter
      break
    }
  }

  cat("  Converged:", sum(converged, na.rm = TRUE), "/", n, "in", actual_iterations, "iterations\n")

  final_TLU           <- current_TLU
  N_manure_per_ha_f   <- (final_TLU * manure_t_per_TLU * collection_rate * N_content_manure * 1000) / maize_ha_total
  P_manure_per_ha_f   <- (final_TLU * manure_t_per_TLU * collection_rate * P_content_manure * 1000) / maize_ha_total

  final_total_N <- (N_manure_per_ha_f + N_litter_per_ha + N_legumes_per_ha) * maize_ha_total
  final_total_P <- (P_manure_per_ha_f + P_litter_per_ha) * maize_ha_total

  final_N_total_per_ha  <- final_total_N / maize_ha_total
  final_P_total_per_ha  <- final_total_P / maize_ha_total
  final_maize_prod      <- total_maize_prod
  final_maize_yield_avg <- final_maize_prod / maize_ha_total
  final_maize_residues  <- final_maize_prod * residue_to_grain_ratio * residue_degradation * residue_feed_share

  results_complete <- data.frame(
    scenario_id             = indicators_results$scenario_id,
    CC_grazing              = CC_grazing,
    maize_ha_total          = maize_ha_total,
    convergence_iteration   = convergence_iteration,
    final_TLU               = final_TLU,
    final_N_total_per_ha    = final_N_total_per_ha,
    final_P_total_per_ha    = final_P_total_per_ha,
    final_N_manure_per_ha   = N_manure_per_ha_f,
    final_P_manure_per_ha   = P_manure_per_ha_f,
    final_total_N           = final_total_N,
    final_total_P           = final_total_P,
    N_density               = final_total_N / TOTAL_AREA_HA,
    P_density               = final_total_P / TOTAL_AREA_HA,
    final_maize_production  = final_maize_prod,
    final_maize_yield_avg   = final_maize_yield_avg,
    final_maize_residues    = final_maize_residues,
    residues_limiting       = final_TLU < (CC_grazing * 0.99),
    grazing_limiting        = final_TLU >= (CC_grazing * 0.99),
    limiting_TLU_factor     = ifelse(final_TLU < CC_grazing * 0.99, "RESIDUES", "GRAZING"),
    TLU_reduction           = (CC_grazing - final_TLU) / CC_grazing
  )

  indicators_results <- merge(
    indicators_results,
    results_complete[, c("scenario_id", "final_TLU", "limiting_TLU_factor",
                         "final_maize_yield_avg", "TLU_reduction",
                         "final_N_manure_per_ha", "final_P_manure_per_ha",
                         "final_maize_production", "final_total_N", "final_total_P",
                         "N_density", "P_density", "final_N_total_per_ha", "final_P_total_per_ha")],
    by = "scenario_id", all.x = TRUE
  )

  min_TLU <- min(indicators_results$final_TLU, na.rm = TRUE)
  max_TLU <- max(indicators_results$final_TLU, na.rm = TRUE)

  indicators_results <- indicators_results %>%
    mutate(
      system_performance_score = (final_TLU - min_TLU) / (max_TLU - min_TLU),
      final_total_N_manure     = final_N_manure_per_ha * maize_ha_total,
      final_total_P_manure     = final_P_manure_per_ha * maize_ha_total
    )

  return(list(
    indicators_results   = indicators_results,
    convergence_history  = convergence_history,
    results_complete     = results_complete
  ))
}
