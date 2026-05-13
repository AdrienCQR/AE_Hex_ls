# Extract tree hedges from cropland and grassland land use classes.
# Creates a new 'tree_hedges' class based on fixed cover fractions.
extract_tree_hedges <- function(scenarios_df, tree_hedge_cropland, tree_hedge_grassland) {
  required_cols <- c("cropland", "grassland")
  missing_cols <- setdiff(required_cols, names(scenarios_df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  scenarios_processed <- scenarios_df %>%
    mutate(
      hedge_from_cropland  = cropland  * tree_hedge_cropland,
      hedge_from_grassland = grassland * tree_hedge_grassland,
      tree_hedges = hedge_from_cropland + hedge_from_grassland,
      cropland    = cropland  * (1 - tree_hedge_cropland),
      grassland   = grassland * (1 - tree_hedge_grassland)
    ) %>%
    select(-hedge_from_cropland, -hedge_from_grassland) %>%
    relocate(tree_hedges, .after = grassland)

  # Verify that land use proportions still sum to ~1
  lulc_cols <- c("wetland_grassland", "horticulture", "cropland", "mineral_bare_soil",
                 "dense_woodland", "open_woodland", "grassland", "tree_hedges", "urban")
  totals <- scenarios_processed %>%
    select(any_of(lulc_cols)) %>%
    rowwise() %>%
    mutate(total = sum(c_across(everything()), na.rm = TRUE)) %>%
    pull(total)

  if (!all(abs(totals - 1) < 0.001)) {
    warning("Some rows have land use proportions that do not sum to 1.")
  }

  return(scenarios_processed)
}
