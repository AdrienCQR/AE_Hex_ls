# Economic diversification indicator — Shannon entropy over land-use activities.
# Each LULC class maps to a set of livelihood activities; the Shannon index
# measures how evenly activity intensity is distributed across the landscape.
calculate_economic_diversification <- function(scenarios_raw, indicators_results,
                                               maize_share, legume_share, tobacco_share) {
  # Activity matrix: rows = LULC classes, columns = livelihood activities
  create_activity_matrix <- function() {
    activities <- c("livestock_grazing", "crafting_materials", "cereal_production",
                    "legume_production", "cash_crops", "livestock_feed", "horticulture_products",
                    "commercial_services", "extraction_materials", "hunting",
                    "firewood", "ntfp_collection", "tree_litter", "fallow_services")
    landuses   <- c("wetland_grassland", "cropland", "horticulture", "urban",
                    "dense_woodland", "open_woodland", "grassland", "tree_hedges", "mineral_bare_soil")

    m <- matrix(0, nrow = length(landuses), ncol = length(activities),
                dimnames = list(landuses, activities))

    m["wetland_grassland", c("livestock_grazing", "crafting_materials")]                                          <- 1
    m["cropland",          c("cereal_production", "legume_production", "cash_crops", "livestock_feed")]           <- 1
    m["horticulture",      "horticulture_products"]                                                               <- 1
    m["urban",             "commercial_services"]                                                                  <- 1
    m["dense_woodland",    c("hunting", "firewood", "extraction_materials", "ntfp_collection", "tree_litter")]    <- 1
    m["tree_hedges",       c("firewood", "extraction_materials", "ntfp_collection", "tree_litter")]               <- 1
    m["open_woodland",     c("livestock_grazing", "hunting", "firewood", "extraction_materials",
                              "ntfp_collection", "tree_litter")]                                                   <- 1
    m["grassland",         c("livestock_grazing", "crafting_materials", "livestock_feed", "fallow_services")]     <- 1
    m["mineral_bare_soil", "extraction_materials"]                                                                <- 1
    m
  }

  activity_matrix <- create_activity_matrix()
  ordered_landuses <- rownames(activity_matrix)

  shannon_for_scenario <- function(lu_props, m, ms, ls, ts) {
    names(lu_props) <- rownames(m)
    intensities <- colSums(sweep(m, 1, lu_props, "*"))

    # Substitute cropland aggregate by crop-specific shares
    cl <- lu_props["cropland"]
    if (!is.na(cl) && cl > 0) {
      intensities["cereal_production"] <- cl * ms
      intensities["legume_production"] <- cl * ls
      intensities["cash_crops"]        <- cl * ts
      intensities["livestock_feed"]    <- 0
    }

    acts <- intensities[intensities > 1e-9]
    if (length(acts) <= 1 || sum(acts) == 0) return(0)
    p <- acts / sum(acts)
    -sum(p * log(p))
  }

  scenarios_ordered <- scenarios_raw[, ordered_landuses, drop = FALSE]

  raw_scores <- apply(scenarios_ordered, 1, function(row) {
    shannon_for_scenario(as.numeric(row), activity_matrix, maize_share, legume_share, tobacco_share)
  })

  mn <- min(raw_scores); mx <- max(raw_scores)
  norm_scores <- if (mx > mn) (raw_scores - mn) / (mx - mn) else rep(0, length(raw_scores))

  eco_div_results <- data.frame(
    scenario_id                  = scenarios_raw$scenario_id,
    eco_div_shannon_raw          = raw_scores,
    economic_diversification_score = norm_scores
  )

  indicators_results %>% left_join(eco_div_results, by = "scenario_id")
}
