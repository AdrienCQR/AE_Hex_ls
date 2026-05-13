# Biodiversity Habitat Integrity (BHI) indicator.
# Weights each LULC class by its habitat quality (0 = no value, 1 = high value).
calculate_biodiv_indicator <- function(scenarios_long, indicators_results) {
  BHI_lookup <- data.frame(
    LULC_Class = c("cropland", "grassland", "dense_woodland", "open_woodland",
                   "wetland_grassland", "horticulture", "tree_hedges", "urban", "mineral_bare_soil"),
    weight     = c(0.0, 0.5, 1.0, 0.5, 0.5, 0.0, 0.5, 0.0, 0.0)
  )

  BHI_results <- scenarios_long %>%
    left_join(BHI_lookup, by = "LULC_Class") %>%
    mutate(weighted_area = proportion * weight) %>%
    group_by(scenario_id) %>%
    summarise(
      biodiv_hab_raw = sum(weighted_area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(biodiv_score = biodiv_hab_raw)

  indicators_results <- indicators_results %>%
    left_join(BHI_results, by = "scenario_id")

  return(indicators_results)
}
