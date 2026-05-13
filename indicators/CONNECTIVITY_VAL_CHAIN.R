# Value chain connectivity indicator — local food system orientation score.
# Each land use class is assigned an orientation score (1 = market, 6 = subsistence),
# weighted by area. The landscape-level score reflects how subsistence-oriented the
# food system is relative to market integration.
calculate_value_chain_connectivity <- function(indicators_results, maize_share,
                                               legume_share, tobacco_share,
                                               normalization_method = "empirical") {
  activity_orientation <- c(
    cereal_production   = 6, legume_production = 3, cash_crops         = 1,
    livestock_grazing   = 5, crafting_materials = 2, livestock_feed    = 2,
    horticulture_products = 4, extraction_materials = 2, hunting       = 3,
    firewood            = 3, ntfp_collection    = 4, tree_litter       = 4
  )

  mean_score <- function(activities) mean(activity_orientation[activities])

  landuse_scores <- list(
    wetland_grassland  = mean_score(c("livestock_grazing", "crafting_materials")),
    horticulture       = activity_orientation["horticulture_products"],
    dense_woodland     = mean_score(c("hunting", "firewood", "extraction_materials", "ntfp_collection", "tree_litter")),
    open_woodland      = mean_score(c("livestock_grazing", "hunting", "firewood", "extraction_materials", "ntfp_collection", "tree_litter")),
    grassland          = mean_score(c("livestock_grazing", "crafting_materials", "livestock_feed")),
    tree_hedges        = mean_score(c("firewood", "extraction_materials", "ntfp_collection", "tree_litter")),
    mineral_bare_soil  = activity_orientation["extraction_materials"]
  )

  get_orientation <- function(lu) {
    if (lu == "cropland") {
      return(maize_share   * activity_orientation["cereal_production"] +
             legume_share  * activity_orientation["legume_production"]  +
             tobacco_share * activity_orientation["cash_crops"])
    }
    landuse_scores[[lu]]
  }

  required_cols <- c("wetland_grassland_ha", "cropland_ha", "horticulture_ha",
                     "dense_woodland_ha", "open_woodland_ha", "grassland_ha",
                     "tree_hedges_ha", "mineral_bare_soil_ha")
  missing <- setdiff(required_cols, colnames(indicators_results))
  if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

  n <- nrow(indicators_results)
  connectivity_per_ha <- numeric(n)

  for (i in seq_len(n)) {
    areas <- c(
      wetland_grassland = indicators_results$wetland_grassland_ha[i],
      cropland          = indicators_results$cropland_ha[i],
      horticulture      = indicators_results$horticulture_ha[i],
      dense_woodland    = indicators_results$dense_woodland_ha[i],
      open_woodland     = indicators_results$open_woodland_ha[i],
      grassland         = indicators_results$grassland_ha[i],
      tree_hedges       = indicators_results$tree_hedges_ha[i],
      mineral_bare_soil = indicators_results$mineral_bare_soil_ha[i]
    )
    total_orientation     <- sum(sapply(names(areas), function(lu) areas[lu] * get_orientation(lu)), na.rm = TRUE)
    connectivity_per_ha[i] <- total_orientation / TOTAL_AREA_HA
  }

  # Normalisation
  if (normalization_method == "empirical") {
    mn <- min(connectivity_per_ha, na.rm = TRUE)
    mx <- max(connectivity_per_ha, na.rm = TRUE)
    final_score <- if (mx > mn) (connectivity_per_ha - mn) / (mx - mn) else rep(0, n)
  } else if (normalization_method == "theoretical") {
    max_th <- (0.20 * landuse_scores$horticulture) + (0.75 * 4) + (0.05 * landuse_scores$dense_woodland)
    min_th <- 0.05 * landuse_scores$mineral_bare_soil
    rng    <- max_th - min_th
    final_score <- if (rng > 0) pmax(0, pmin(1, (connectivity_per_ha - min_th) / rng)) else rep(0, n)
  } else {
    stop("Unknown normalization_method: choose 'empirical' or 'theoretical'.")
  }

  indicators_results$connectivity_score_brut <- connectivity_per_ha
  indicators_results$connectivity_score       <- final_score

  return(indicators_results)
}
