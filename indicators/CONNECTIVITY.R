# =============================================================================
# CONNECTIVITY.R — Composite Local Food System Connectivity indicator
# =============================================================================
# Operationalises the HLPE (2019) agroecological connectivity principle:
# "ensuring proximity and confidence between producers and consumers through
# promotion of fair and short distribution networks and by re-embedding food
# systems into local economies."
#
# Two complementary dimensions:
#
#   1. OrientationScore  — area-weighted land-use orientation toward local vs.
#      export markets (1 = purely export-oriented, 6 = purely subsistence).
#      Derived from LULC composition of each hex cell.
#
#   2. AccessibilityScore — mean travel time from each land-use class to the
#      nearest market, weighted by class area within the cell (shorter = better).
#      Pre-computed spatially by CONNECTIVITY_PREP.R.
#
# Final score (normalised to [0, 1]):
#   connectivity_score = alpha × OrientationScore + (1 - alpha) × AccessibilityScore
#
# Only when BOTH dimensions are high does a landscape show strong, functional
# local connectivity (see interpretation matrix in the documentation).
#
# Requires: CONNECTIVITY_PREP.R to have been run (accessibility CSV must exist).
# =============================================================================

calculate_connectivity <- function(
  indicators_results,
  maize_share,
  legume_share,
  tobacco_share,
  accessibility_file,
  alpha = 0.5,
  normalization_method = "empirical"   # "empirical" | "theoretical"
) {

  # ===========================================================================
  # 1. Orientation Score
  # ===========================================================================
  # Activity-level orientation scale: 1 = most market/export, 6 = most local/subsistence
  activity_orientation <- c(
    cereal_production      = 6,   # staple food for household consumption
    legume_production      = 3,   # dual purpose: subsistence + some market
    cash_crops             = 1,   # tobacco — fully export-oriented
    livestock_grazing      = 5,   # mainly subsistence / local sales
    crafting_materials     = 2,
    livestock_feed         = 2,
    horticulture_products  = 4,   # sold locally at markets
    extraction_materials   = 2,
    hunting                = 3,
    firewood               = 3,
    ntfp_collection        = 4,
    tree_litter            = 4
  )

  mean_score <- function(acts) mean(activity_orientation[acts])

  landuse_orientation <- list(
    wetland_grassland  = mean_score(c("livestock_grazing", "crafting_materials")),
    horticulture       = unname(activity_orientation["horticulture_products"]),
    dense_woodland     = mean_score(c("hunting", "firewood", "extraction_materials",
                                      "ntfp_collection", "tree_litter")),
    open_woodland      = mean_score(c("livestock_grazing", "hunting", "firewood",
                                      "extraction_materials", "ntfp_collection",
                                      "tree_litter")),
    grassland          = mean_score(c("livestock_grazing", "crafting_materials",
                                      "livestock_feed")),
    tree_hedges        = mean_score(c("firewood", "extraction_materials",
                                      "ntfp_collection", "tree_litter")),
    mineral_bare_soil  = unname(activity_orientation["extraction_materials"])
  )

  get_orientation <- function(lu) {
    if (lu == "cropland") {
      return(
        maize_share   * activity_orientation["cereal_production"] +
        legume_share  * activity_orientation["legume_production"] +
        tobacco_share * activity_orientation["cash_crops"]
      )
    }
    unname(landuse_orientation[[lu]])
  }

  required_cols <- c(
    "wetland_grassland_ha", "cropland_ha", "horticulture_ha",
    "dense_woodland_ha", "open_woodland_ha", "grassland_ha",
    "tree_hedges_ha", "mineral_bare_soil_ha"
  )
  missing <- setdiff(required_cols, colnames(indicators_results))
  if (length(missing) > 0)
    stop("Missing columns in indicators_results: ", paste(missing, collapse = ", "))

  n <- nrow(indicators_results)
  orientation_raw <- numeric(n)

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
    total_ha <- sum(areas, na.rm = TRUE)
    if (total_ha == 0) next
    orientation_raw[i] <- sum(
      sapply(names(areas), function(lu) areas[lu] * get_orientation(lu)),
      na.rm = TRUE
    ) / total_ha
  }

  # ===========================================================================
  # 2. Accessibility Score — load pre-computed spatial values
  # ===========================================================================

  if (!file.exists(accessibility_file)) {
    stop(
      "[CONNECTIVITY] Accessibility file not found:\n  ", accessibility_file,
      "\nPlease run CONNECTIVITY_PREP.R first (or check FILE_PATHS$connectivity_access)."
    )
  }

  acc_data <- read.csv(accessibility_file)
  acc_data$scenario_id <- as.character(acc_data$scenario_id)

  indicators_results$scenario_id <- as.character(indicators_results$scenario_id)
  indicators_results <- left_join(
    indicators_results,
    acc_data %>% dplyr::select(scenario_id, accessibility_score_brut),
    by = "scenario_id"
  )

  n_missing_acc <- sum(is.na(indicators_results$accessibility_score_brut))
  if (n_missing_acc > 0)
    warning("[CONNECTIVITY] ", n_missing_acc,
            " cells have no accessibility score — they will have NA connectivity.")

  accessibility_raw <- indicators_results$accessibility_score_brut

  # ===========================================================================
  # 3. Normalise both dimensions to [0, 1]
  # ===========================================================================

  normalize_minmax <- function(x, reverse = FALSE) {
    mn <- min(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    if (mx == mn) return(rep(0.5, length(x)))
    norm <- (x - mn) / (mx - mn)
    if (reverse) 1 - norm else norm
  }

  if (normalization_method == "empirical") {
    # Orientation: higher = more subsistence-oriented = better → keep direction
    orientation_score  <- normalize_minmax(orientation_raw,  reverse = FALSE)
    # Accessibility: shorter travel time = better → reverse so high score = close
    accessibility_score <- normalize_minmax(accessibility_raw, reverse = TRUE)

  } else if (normalization_method == "theoretical") {
    # Orientation theoretical bounds: all export (score ≈ 1) to all subsistence (score = 6)
    ori_min <- 1; ori_max <- 6
    orientation_score <- pmax(0, pmin(1, (orientation_raw - ori_min) / (ori_max - ori_min)))

    # Accessibility theoretical bounds: 0 h (on-market) to e.g. 4 h max reasonable
    acc_max_th <- 4
    accessibility_score <- pmax(0, pmin(1, 1 - accessibility_raw / acc_max_th))

  } else {
    stop("Unknown normalization_method: choose 'empirical' or 'theoretical'.")
  }

  # ===========================================================================
  # 4. Composite score
  # ===========================================================================
  # Equal or adjustable weight between orientation and accessibility.
  # Both must be high for strong functional local connectivity
  # (subsistence-oriented AND physically close to markets).

  connectivity_score <- alpha * orientation_score + (1 - alpha) * accessibility_score

  # ===========================================================================
  # 5. Write output columns
  # ===========================================================================
  # Raw sub-scores are kept for interpretation / sensitivity analysis.
  # The intermediate [0,1] normalised sub-scores are NOT exposed: the internal
  # normalisation is only needed to put hours and the 1–6 orientation scale on
  # a common footing before combining them. The final normalisation to [0,1]
  # happens downstream in the AE composite step.

  indicators_results$orientation_score_brut  <- orientation_raw      # raw (1–6 scale)
  # accessibility_score_brut already joined above (raw travel time in hours)

  # connectivity_score_brut is kept as the composite for downstream compatibility
  # (AE composite, synergy analysis, and maps all reference this column)
  indicators_results$connectivity_score_brut  <- connectivity_score    # composite [0,1]

  return(indicators_results)
}
