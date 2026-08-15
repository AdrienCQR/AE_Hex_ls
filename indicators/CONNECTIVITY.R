# =============================================================================
# CONNECTIVITY.R — Physical (road-network) Connectivity indicator
# =============================================================================
# Operationalises the HLPE (2019) agroecological connectivity principle:
# "ensuring proximity and confidence between producers and consumers through
# promotion of fair and short distribution networks and by re-embedding food
# systems into local economies."
#
# Physical connectivity only: mean travel time (decimal hours) from each hex
# cell to the nearest market, via the road-network friction / cost-distance
# surface pre-computed by CONNECTIVITY_PREP.R. Lower travel time = better
# connected.
#
# NOTE — land-use orientation dimension removed (2026-08-15):
# A previous version of this indicator also computed a land-use "orientation"
# score (subsistence- vs. export-orientation of crops/land uses) and blended
# it with the accessibility score into a composite [0,1] index:
#   connectivity_score = alpha * OrientationScore + (1 - alpha) * AccessibilityScore
# That logic is disabled below (commented out) for reference/rollback. The
# full original implementation is also backed up at
# indicators/CONNECTIVITY_isoch_orientation.R.
#
# Requires: CONNECTIVITY_PREP.R to have been run (accessibility CSV must exist).
# =============================================================================

calculate_connectivity <- function(
  indicators_results,
  accessibility_file
  # --- Arguments only needed for the disabled orientation score, see below ---
  # , maize_share
  # , legume_share
  # , tobacco_share
  # , alpha = 0.5
  # , normalization_method = "empirical"   # "empirical" | "theoretical"
) {

  # ===========================================================================
  # 1. Orientation Score — DISABLED, kept for rollback reference
  # ===========================================================================
  # # Activity-level orientation scale: 1 = most market/export, 6 = most local/subsistence
  # activity_orientation <- c(
  #   cereal_production      = 6,   # staple food for household consumption
  #   legume_production      = 3,   # dual purpose: subsistence + some market
  #   cash_crops             = 1,   # tobacco — fully export-oriented
  #   livestock_grazing      = 5,   # mainly subsistence / local sales
  #   crafting_materials     = 2,
  #   livestock_feed         = 2,
  #   horticulture_products  = 4,   # sold locally at markets
  #   extraction_materials   = 2,
  #   hunting                = 3,
  #   firewood               = 3,
  #   ntfp_collection        = 4,
  #   tree_litter            = 4
  # )
  #
  # mean_score <- function(acts) mean(activity_orientation[acts])
  #
  # landuse_orientation <- list(
  #   wetland_grassland  = mean_score(c("livestock_grazing", "crafting_materials")),
  #   horticulture       = unname(activity_orientation["horticulture_products"]),
  #   dense_woodland     = mean_score(c("hunting", "firewood", "extraction_materials",
  #                                     "ntfp_collection", "tree_litter")),
  #   open_woodland      = mean_score(c("livestock_grazing", "hunting", "firewood",
  #                                     "extraction_materials", "ntfp_collection",
  #                                     "tree_litter")),
  #   grassland          = mean_score(c("livestock_grazing", "crafting_materials",
  #                                     "livestock_feed")),
  #   tree_hedges        = mean_score(c("firewood", "extraction_materials",
  #                                     "ntfp_collection", "tree_litter")),
  #   mineral_bare_soil  = unname(activity_orientation["extraction_materials"])
  # )
  #
  # get_orientation <- function(lu) {
  #   if (lu == "cropland") {
  #     return(
  #       maize_share   * activity_orientation["cereal_production"] +
  #       legume_share  * activity_orientation["legume_production"] +
  #       tobacco_share * activity_orientation["cash_crops"]
  #     )
  #   }
  #   unname(landuse_orientation[[lu]])
  # }
  #
  # required_cols <- c(
  #   "wetland_grassland_ha", "cropland_ha", "horticulture_ha",
  #   "dense_woodland_ha", "open_woodland_ha", "grassland_ha",
  #   "tree_hedges_ha", "mineral_bare_soil_ha"
  # )
  # missing <- setdiff(required_cols, colnames(indicators_results))
  # if (length(missing) > 0)
  #   stop("Missing columns in indicators_results: ", paste(missing, collapse = ", "))
  #
  # n <- nrow(indicators_results)
  # orientation_raw <- numeric(n)
  #
  # for (i in seq_len(n)) {
  #   areas <- c(
  #     wetland_grassland = indicators_results$wetland_grassland_ha[i],
  #     cropland          = indicators_results$cropland_ha[i],
  #     horticulture      = indicators_results$horticulture_ha[i],
  #     dense_woodland    = indicators_results$dense_woodland_ha[i],
  #     open_woodland     = indicators_results$open_woodland_ha[i],
  #     grassland         = indicators_results$grassland_ha[i],
  #     tree_hedges       = indicators_results$tree_hedges_ha[i],
  #     mineral_bare_soil = indicators_results$mineral_bare_soil_ha[i]
  #   )
  #   total_ha <- sum(areas, na.rm = TRUE)
  #   if (total_ha == 0) next
  #   orientation_raw[i] <- sum(
  #     sapply(names(areas), function(lu) areas[lu] * get_orientation(lu)),
  #     na.rm = TRUE
  #   ) / total_ha
  # }

  # ===========================================================================
  # 2. Accessibility Score — load pre-computed spatial values (road network)
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

  # ===========================================================================
  # 3. Normalise + blend with orientation — DISABLED, kept for rollback reference
  # ===========================================================================
  # normalize_minmax <- function(x, reverse = FALSE) {
  #   mn <- min(x, na.rm = TRUE)
  #   mx <- max(x, na.rm = TRUE)
  #   if (mx == mn) return(rep(0.5, length(x)))
  #   norm <- (x - mn) / (mx - mn)
  #   if (reverse) 1 - norm else norm
  # }
  #
  # accessibility_raw <- indicators_results$accessibility_score_brut
  #
  # # Normalisation method choice: empirical = default, based on observed min/max in the dataset; theoretical = based on predefined bounds.
  # if (normalization_method == "empirical") {
  #   # Orientation: higher = more subsistence-oriented = better → keep direction
  #   orientation_score  <- normalize_minmax(orientation_raw,  reverse = FALSE)
  #   # Accessibility: shorter travel time = better → reverse so high score = close
  #   accessibility_score <- normalize_minmax(accessibility_raw, reverse = TRUE)
  #
  # } else if (normalization_method == "theoretical") {
  #   # Orientation theoretical bounds: all export (score ≈ 1) to all subsistence (score = 6)
  #   ori_min <- 1; ori_max <- 6
  #   orientation_score <- pmax(0, pmin(1, (orientation_raw - ori_min) / (ori_max - ori_min)))
  #
  #   # Accessibility theoretical bounds: 0 h (on-market) to e.g. 4 h max reasonable
  #   acc_max_th <- 4
  #   accessibility_score <- pmax(0, pmin(1, 1 - accessibility_raw / acc_max_th))
  #
  # } else {
  #   stop("Unknown normalization_method: choose 'empirical' or 'theoretical'.")
  # }
  #
  # # Equal or adjustable weight between orientation and accessibility.
  # # Both must be high for strong functional local connectivity
  # # (subsistence-oriented AND physically close to markets).
  # connectivity_score <- alpha * orientation_score + (1 - alpha) * accessibility_score
  #
  # indicators_results$orientation_score_brut  <- orientation_raw      # raw (1–6 scale)

  # ===========================================================================
  # 4. Write output column
  # ===========================================================================
  # Final connectivity indicator = raw physical accessibility, i.e. mean
  # travel time to the nearest market (decimal hours). Lower = better
  # connected. Kept under the same column name for downstream compatibility
  # (AE composite, synergy analysis, and maps all reference this column) —
  # note it is no longer normalised to [0,1] as it was in the composite version.
  indicators_results$connectivity_score_brut <- indicators_results$accessibility_score_brut

  return(indicators_results)
}
