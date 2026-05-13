# N fixation from legume-maize rotation (groundnut).
# Biomass and yield values from RAIZ experimental data.
calculate_N_fixed <- function(indicators_results, NDFA_rate, HI_gdnuts, legume_share) {
  above    <- 2907          # aboveground biomass (kg/ha)
  grain    <- 688           # grain yield (kg/ha)
  roots    <- 0.3 * above   # root biomass (30% of aboveground)
  residues <- above + roots

  N_residues <- residues * 0.02        # 2% N in residues
  N_grains   <- grain   * 0.038       # 3.8% N in grain
  N_uptake   <- N_residues + N_grains
  NDFA       <- N_uptake * NDFA_rate  # N derived from atmosphere
  N_exported <- NDFA * HI_gdnuts      # N exported in harvested grain
  N_fixed    <- NDFA - N_exported     # N remaining in system
  N_legumes_fixed <- N_fixed * legume_share

  indicators_results$total_gdnut_grain_prod <- grain * legume_share * indicators_results$cropland_ha
  indicators_results$N_legumes_fixed        <- N_legumes_fixed
  indicators_results$total_N_legumes        <- N_legumes_fixed * indicators_results$maize_ha

  return(indicators_results)
}
