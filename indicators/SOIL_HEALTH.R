# Soil Organic Carbon (SOC) indicator.
# SOC reference values per LULC class (t C/ha, 0-20 cm), adapted from Nyawasha et al. (2025).
calculate_soc_indicator <- function(scenarios_long, indicators_results) {
  soc_lookup <- data.frame(
    LULC_Class = c("cropland", "grassland", "open_woodland", "dense_woodland",
                   "wetland_grassland", "horticulture", "tree_hedges", "other"),
    soc_ha     = c(14.5, 11.0, 14.4, 12.9, 39.9, 32.1, 12.9, 0)
  )

  soc_by_scenario <- scenarios_long %>%
    left_join(soc_lookup, by = "LULC_Class") %>%
    mutate(
      soc_ha        = ifelse(is.na(soc_ha), 0, soc_ha),
      soc_total_lulc = soc_ha * area_ha
    ) %>%
    group_by(scenario_id) %>%
    summarise(soc_stocks = round(sum(soc_total_lulc, na.rm = TRUE), 2), .groups = "drop") %>%
    mutate(
      min_soc        = min(soc_stocks, na.rm = TRUE),
      max_soc        = max(soc_stocks, na.rm = TRUE),
      soc_normalized = round((soc_stocks - min_soc) / (max_soc - min_soc), 3)
    )

  indicators_results <- indicators_results %>%
    left_join(soc_by_scenario %>% select(scenario_id, soc_stocks, soc_normalized), by = "scenario_id") %>%
    mutate(soc_density_ha = round(soc_stocks / TOTAL_AREA_HA, 2))

  return(indicators_results)
}
