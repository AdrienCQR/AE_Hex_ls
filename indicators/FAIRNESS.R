# Fairness indicator — weighted Gini coefficient over stakeholder satisfaction scores.
# Satisfaction = dot product of group land-use preferences and landscape composition.
# Fairness score = 1 - Gini (higher = more equitable distribution of satisfaction).
calculate_fairness_weighted <- function(scenarios_raw, indicators_results) {
  preferences_file <- "data/raw/data_indicators/RESULTS_mean_pref_LU_groups.csv"
  if (!file.exists(preferences_file)) {
    stop("Preferences file not found: ", preferences_file)
  }

  fairness_prefs <- read.csv(preferences_file, stringsAsFactors = FALSE) %>%
    select(-any_of("method"))

  # Map landscape classes to preference table column names
  scenarios_lu <- scenarios_raw %>%
    mutate(
      cereals        = cropland * maize_share,
      legumes        = cropland * legume_share,
      cash_crops     = cropland * tobacco_share,
      dense_woodlands = dense_woodland,
      open_woodlands  = open_woodland,
      trees_hedges    = tree_hedges,
      gardens         = horticulture,
      vleis           = wetland_grassland,
      makuras         = grassland
    ) %>%
    select(scenario_id, dense_woodlands, open_woodlands, trees_hedges,
           cereals, legumes, cash_crops, gardens, vleis, makuras, urban)

  lu_cols     <- setdiff(names(fairness_prefs), "NEW_GROUPS")
  common_cols <- intersect(lu_cols, setdiff(names(scenarios_lu), "scenario_id"))
  missing     <- setdiff(lu_cols, common_cols)
  if (length(missing) > 0) stop("Missing scenario columns: ", paste(missing, collapse = ", "))

  # Stakeholder group weights (higher = greater influence on fairness score)
  group_weights <- c(
    farmers = 3, hunter_fisherman = 3, services_and_trades = 3,
    crop_manager = 2, environmental_manager = 2, forestry_manager = 2,
    horticulture_manager = 2, livestock_manager = 2,
    researchers = 1
  )

  n <- nrow(scenarios_lu)
  gini_coefficients <- numeric(n)

  for (i in seq_len(n)) {
    composition    <- as.numeric(scenarios_lu[i, common_cols])
    satisfactions  <- sapply(seq_len(nrow(fairness_prefs)), function(j) {
      prefs <- as.numeric(fairness_prefs[j, common_cols])
      sum(prefs * composition)
    })
    names(satisfactions) <- fairness_prefs$NEW_GROUPS

    w  <- group_weights[fairness_prefs$NEW_GROUPS]
    w  <- w / sum(w)
    ng <- length(satisfactions)

    total_diff <- sum(outer(seq_len(ng), seq_len(ng), function(k, l) {
      w[k] * w[l] * abs(satisfactions[k] - satisfactions[l])
    }))

    mean_sat <- sum(w * satisfactions)
    gini_coefficients[i] <- total_diff / (2 * mean_sat)
  }

  raw_fairness  <- 1 - gini_coefficients
  mn <- min(raw_fairness); mx <- max(raw_fairness)
  norm_fairness <- (raw_fairness - mn) / (mx - mn)

  indicators_results$equity_raw   <- raw_fairness
  indicators_results$equity_score <- norm_fairness

  return(indicators_results)
}
