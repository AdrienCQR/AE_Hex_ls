# Grass biomass, grazing carrying capacity, and nutrient returns from manure.
calculate_grazing_capacity <- function(scenarios_long, indicators_results,
                                       RUE, R, wet_months, cattle_daily_intake,
                                       cattle_weight, TOTAL_AREA_HA) {
  # Basal area factor A per LULC class (used in biomass model)
  lulc_table <- data.frame(
    LULC_Class = c("wetland_grassland", "cropland", "horticulture", "grassland",
                   "dense_woodland", "open_woodland", "urban", "mineral_bare_soil"),
    A = c(0.01, NA, NA, 0.01, 10, 5, NA, NA)
  )

  merged_data <- merge(scenarios_long, lulc_table, by = "LULC_Class", all.x = TRUE)
  merged_data$A[is.na(merged_data$A)] <- 0

  # Grassland and wetland: biomass = RUE * R
  # Woody classes: biomass from allometric model (1878 * A^-0.45)
  grass_classes <- c("wetland_grassland", "grassland")
  merged_data$Biomass_kg_ha <- ifelse(
    merged_data$LULC_Class %in% grass_classes,
    RUE * R,
    suppressWarnings(1878 * merged_data$A^-0.45)
  )
  merged_data$Biomass_kg_ha[is.na(merged_data$Biomass_kg_ha) | is.infinite(merged_data$Biomass_kg_ha)] <- 0
  merged_data$total_grass_biomass <- merged_data$Biomass_kg_ha * merged_data$area_ha

  total_biomass_df <- merged_data %>%
    group_by(scenario_id) %>%
    summarise(total_grass_biomass = sum(total_grass_biomass, na.rm = TRUE), .groups = "drop")

  # Wet-season carrying capacity (TLU)
  daily_need_kg        <- cattle_daily_intake * cattle_weight
  days_wet             <- (365 / 12) * wet_months
  wet_feed_per_TLU     <- daily_need_kg * days_wet

  total_biomass_df$max_grazing_CC_TLU <- round(total_biomass_df$total_grass_biomass / wet_feed_per_TLU, 2)

  # Manure nutrient returns
  N_content <- 0.014; P_content <- 0.002; K_content <- 0.006; coll_rate <- 0.5
  total_biomass_df$Manure_ton       <- total_biomass_df$max_grazing_CC_TLU * 1.022 * coll_rate
  total_biomass_df$total_grazing_Nkg <- total_biomass_df$Manure_ton * N_content * 1000
  total_biomass_df$total_grazing_Pkg <- total_biomass_df$Manure_ton * P_content * 1000
  total_biomass_df$total_grazing_Kkg <- total_biomass_df$Manure_ton * K_content * 1000

  indicators_results <- merge(
    indicators_results,
    total_biomass_df[, c("scenario_id", "total_grass_biomass", "max_grazing_CC_TLU",
                         "total_grazing_Nkg", "total_grazing_Pkg", "total_grazing_Kkg")],
    by = "scenario_id", all.x = TRUE
  )

  indicators_results$init_grazing_N_kg_per_maize_ha <- indicators_results$total_grazing_Nkg / indicators_results$maize_ha
  indicators_results$init_grazing_P_kg_per_maize_ha <- indicators_results$total_grazing_Pkg / indicators_results$maize_ha

  # Empirical min-max normalisation of grass biomass score
  min_b <- min(indicators_results$total_grass_biomass, na.rm = TRUE)
  max_b <- max(indicators_results$total_grass_biomass, na.rm = TRUE)
  indicators_results$grass_score <- if (max_b > min_b) {
    pmax(0, pmin(1, (indicators_results$total_grass_biomass - min_b) / (max_b - min_b)))
  } else {
    rep(0, nrow(indicators_results))
  }

  return(indicators_results)
}
