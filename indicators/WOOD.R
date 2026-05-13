# Sustainable firewood availability indicator.
# Annual wood yield = aboveground biomass / regeneration years * woody coefficient.
# Net availability is computed after deducting wood used for tobacco curing.
calculate_wood_indicators <- function(scenarios_long, woody_coefficient, regeneration_years,
                                      HH_number, HH_wood_demand, indicators_results,
                                      wood_demand_per_ha_tobacco, tobacco_share, TOTAL_AREA_HA) {
  AGB_median <- read.csv("data/processed/AGB_median_2010.csv")

  AGB_lookup <- data.frame(
    LULC_Class = c("cropland", "horticulture", "grassland", "wetland_grassland",
                   "dense_woodland", "tree_hedges", "open_woodland", "urban", "mineral_bare_soil")
  ) %>%
    mutate(
      AGB_median = case_when(
        LULC_Class %in% c("dense_woodland", "tree_hedges") ~
          AGB_median$AGB_median[AGB_median$class_name == "dense_woodland"],
        LULC_Class == "open_woodland" ~
          AGB_median$AGB_median[AGB_median$class_name == "woodland"],
        TRUE ~ 0
      ),
      annual_wood_yield = AGB_median / regeneration_years * woody_coefficient
    )

  wood_by_scenario <- scenarios_long %>%
    left_join(AGB_lookup, by = "LULC_Class") %>%
    mutate(wood_available_lulc = annual_wood_yield * area_ha) %>%
    group_by(scenario_id) %>%
    summarise(total_wood_available = round(sum(wood_available_lulc, na.rm = TRUE), 2), .groups = "drop")

  # Empirical normalisation
  mn <- min(wood_by_scenario$total_wood_available, na.rm = TRUE)
  mx <- max(wood_by_scenario$total_wood_available, na.rm = TRUE)
  wood_by_scenario <- wood_by_scenario %>%
    mutate(
      wood_availability_score = if (mx > mn) {
        pmax(0, pmin(1, (total_wood_available - mn) / (mx - mn)))
      } else { 0.5 }
    )

  # Deduct tobacco curing demand
  tobacco_area     <- indicators_results$cropland_ha * tobacco_share
  tobacco_wood_dem <- data.frame(
    scenario_id          = indicators_results$scenario_id,
    tobacco_wood_demand  = tobacco_area * wood_demand_per_ha_tobacco
  )

  domestic_wood_demand <- HH_number * HH_wood_demand

  wood_by_scenario <- wood_by_scenario %>%
    left_join(tobacco_wood_dem, by = "scenario_id") %>%
    mutate(
      domestic_wood_demand             = domestic_wood_demand,
      wood_available_post_tobacco      = total_wood_available - tobacco_wood_demand,
      sustainable_wood_per_household   = wood_available_post_tobacco / HH_number,
      sustainable_wood_per_farming_hh  = wood_available_post_tobacco / (HH_number * 0.7),
      wood_balance_status = case_when(
        wood_available_post_tobacco < 0                ~ "Critical deficit",
        wood_available_post_tobacco < domestic_wood_demand ~ "Insufficient for domestic use",
        TRUE                                            ~ "Surplus"
      ),
      total_wood_demand  = domestic_wood_demand + tobacco_wood_demand,
      total_wood_ratio   = total_wood_available / total_wood_demand,
      domestic_wood_ratio = total_wood_available / domestic_wood_demand
    )

  indicators_results %>%
    left_join(
      wood_by_scenario %>%
        select(scenario_id, wood_availability_score, sustainable_wood_per_household,
               sustainable_wood_per_farming_hh, wood_available_post_tobacco,
               wood_balance_status, domestic_wood_demand, tobacco_wood_demand,
               total_wood_available, domestic_wood_ratio, total_wood_ratio),
      by = "scenario_id"
    )
}
