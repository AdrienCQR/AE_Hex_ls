# N and P inputs from tree litter (dense woodland, open woodland, tree hedges).
# Collectable litter is capped at household collection capacity.
calculate_N_litter <- function(scenarios_long, indicators_results,
                               HH_number, maize_share, collectable_litter_per_HH) {
  litter_dense      <- 3.23          # t litter/ha/yr — dense woodland
  litter_open       <- 3.23 / 2     # t litter/ha/yr — open woodland
  N_litter_content  <- 0.015        # kg N / kg litter
  P_litter_content  <- 0.0008       # kg P / kg litter

  litter_by_scenario <- scenarios_long %>%
    mutate(
      litter_per_ha = case_when(
        LULC_Class == "dense_woodland" ~ litter_dense,
        LULC_Class == "open_woodland"  ~ litter_open,
        LULC_Class == "tree_hedges"    ~ litter_dense,
        TRUE ~ 0
      ),
      litter = area_ha * litter_per_ha
    ) %>%
    group_by(scenario_id) %>%
    summarise(total_litter = sum(litter, na.rm = TRUE), .groups = "drop")

  collectable_total_litter <- HH_number * collectable_litter_per_HH

  indicators_results <- indicators_results %>%
    left_join(litter_by_scenario, by = "scenario_id") %>%
    mutate(
      maize_ha               = cropland_ha * maize_share,
      actual_litter_used     = pmin(total_litter, collectable_total_litter),
      ls_N_litter            = total_litter * N_litter_content * 1000,
      ls_P_litter            = total_litter * P_litter_content * 1000,
      total_N_litter_kg      = actual_litter_used * N_litter_content * 1000,
      total_P_litter_kg      = actual_litter_used * P_litter_content * 1000,
      N_litter_kg_per_maize_ha = ifelse(maize_ha > 0, total_N_litter_kg / maize_ha, 0),
      P_litter_kg_per_maize_ha = ifelse(maize_ha > 0, total_P_litter_kg / maize_ha, 0)
    )

  return(indicators_results)
}
