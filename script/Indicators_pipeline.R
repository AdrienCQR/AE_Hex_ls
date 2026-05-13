# RUN_ME.R

# LANCEZ CE SCRIPT POUR CALCULER TOUS VOS INDICATEURS LULC
# ========

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(GGally)
library(cowplot)
library(COINr)
library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(glue)
library(COINr)
library(GGally)

cat("🚀 DÉMARRAGE DU WORKFLOW INDICATEURS LULC\n")
cat("==========================================\n")

rm(list = ls()) # Nettoyage de l'environnement


# ==========================================
# 1. CHARGEMENT PARAMÈTRES
# ==========================================
cat("📋 Chargement paramètres...\n")
source("AE_Hex_ls/script/config_param_AE_hex.R")

# Création dossiers si inexistants
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

cat("✅ Paramètres chargés - Version:", VERSION, "\n")

# ==========================================
# 2. CHARGEMENT DONNÉES
# ==========================================
cat("📂 Chargement scénarios LULC...\n")

# Vérification fichier existe
if (!file.exists(FILE_PATHS$input_scenarios)) {
  stop("❌ ERREUR: Fichier non trouvé: ", FILE_PATHS$input_scenarios)
}


scenarios_raw <- read.csv(
  "AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv"
)
scenarios_raw$scenario_id <- as.character(scenarios_raw$scenario_id)
baseline <- read.csv2("data/processed/baseline_compo.csv")
scenarios_raw <- bind_rows(baseline, scenarios_raw)
str(baseline)
str(scenarios_raw)

summary(scenarios_raw$cropland)


# ==========================================
# 3. RUN all individual indicators
# ==========================================

source("R/run_pipeline.R")

res <- run_all_indicators(
  scenarios_raw = scenarios_raw,
  INDICATORS_TO_RUN = INDICATORS_TO_RUN,
  params = list(
    # généraux
    TOTAL_AREA_HA = TOTAL_AREA_HA,
    tree_hedge_cropland = tree_hedge_cropland,
    tree_hedge_grassland = tree_hedge_grassland,

    # N_LITTER_TREES
    HH_number = HH_number,
    maize_share = maize_share,
    collectable_litter_per_HH = collectable_litter_per_HH,

    # N_LEGUMES_ROTATION
    NDFA_rate = NDFA_rate,
    HI_gdnuts = HI_gdnuts,
    legume_share = legume_share,

    # GRAZING_CC
    RUE = RUE,
    R = R,
    wet_months = wet_months,
    cattle_daily_intake = cattle_daily_intake,
    cattle_weight = cattle_weight,

    # AGRO_PASTO_LOOP
    tolerance = tolerance,
    max_iterations = max_iterations,
    residue_to_grain_ratio = residue_to_grain_ratio,
    residue_degradation = residue_degradation,
    residue_feed_share = residue_feed_share,
    winter_months = winter_months,
    damage_level = damage_level,

    # ECO_DIV
    tobacco_share = tobacco_share,

    # FSN
    total_pop = total_pop,
    HH_wood_demand = HH_wood_demand,
    HH_farming = HH_farming,
    min_diversity_target = min_diversity_target,
    threshold_woodland_ha_per_capita = threshold_woodland_ha_per_capita,

    # WOOD
    woody_coefficient = woody_coefficient,
    regeneration_years = regeneration_years,
    wood_demand_per_ha_tobacco = wood_demand_per_ha_tobacco
  ),
  # options du score SYNERGY (peuvent rester par défaut)
  synergy_opts = list(
    palette = "viridis::magma",
    direction = -1,
    style = "quantile",
    K = 4
  ),
  print_synergy_plot = TRUE
)

# Récupération des sorties
scenarios_raw <- res$scenarios_processed
scenarios_long <- res$scenarios_long
indicators_results <- res$indicators_results
# (le ridge plot a déjà été affiché si print_synergy_plot = TRUE ; sinon: print(res$plots$synergy))

summary(indicators_results)
summary(scenarios_raw)


scenarios_raw$total <- rowSums(scenarios_raw[, -1], na.rm = TRUE)


# ==========================================
# 4.2 Composite indicators
# ==========================================

# Créer une table composite_indicators avec scenario_id depuis indicators_results
composite_indicators <- indicators_results %>%
  select(scenario_id)


# Assurez-vous que la librairie dplyr est chargée
library(dplyr)

# ==========================================
# 4.2 Composite indicators
# ==========================================

# --- 1) Définir les noms des colonnes à inclure dans l'indicateur composite
# On prend les noms originaux des colonnes depuis 'indicators_results'
# et on y ajoute le score de synergie calculé précédemment.
ae_indicators <- c(
  "final_N_total_per_ha", # RECYCLING
  "total_N_legumes", # INPUT_USE
  "soc_density_ha", # SOIL_HEALTH
  "biodiv_score", # BIODIV
  "eco_div_shannon_raw", # ECONOMIC_DIV
  "equity_raw", # FAIRNESS
  "connectivity_score_brut", # CONNECT
  "synergy_score_sum" # SYNERGY
)


# --- 2) Créer une fonction de normalisation min-max
# Cette fonction prend un vecteur et le normalise sur une échelle de 0 à 1.
# Elle gère le cas où toutes les valeurs sont identiques pour éviter une division par zéro.
normalize_min_max <- function(x) {
  # S'il n'y a pas de variation, on retourne 0 pour éviter NaN. On pourrait aussi retourner 0.5.
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


# --- 3) Calculer l'indicateur composite
composite_indicators <- indicators_results %>%

  # Sélectionner uniquement les colonnes nécessaires pour le calcul
  select(scenario_id, all_of(ae_indicators)) %>%

  # Normaliser chaque indicateur de 0 à 1 et créer de nouvelles colonnes
  # Le .names = "{.col}_norm" crée, par exemple, 'soc_stocks_norm'
  mutate(across(
    all_of(ae_indicators),
    normalize_min_max,
    .names = "{.col}_norm"
  )) %>%

  # Passer en mode 'row-wise' pour calculer la moyenne géométrique pour chaque scénario
  rowwise() %>%

  # Calculer la moyenne géométrique des indicateurs normalisés
  # Note : la moyenne géométrique est exp(mean(log(x))).
  # Si une valeur normalisée est 0, log(0) = -Inf, et le score final sera 0.
  # C'est un comportement souhaité qui pénalise fortement une défaillance totale.
  mutate(
    ae_composite_score = exp(mean(log(c_across(ends_with("_norm")) + 0.01)))
  ) %>%

  # Retirer le mode 'row-wise' pour les opérations futures
  ungroup() %>%

  # Sélectionner les colonnes finales pour la clarté
  select(scenario_id, ae_composite_score, ends_with("_norm"))


# Afficher les premières lignes du résultat pour vérifier
print(head(composite_indicators))
summary(composite_indicators$ae_composite_score)
plot(
  composite_indicators$ae_composite_score,
  composite_indicators$final_N_total_per_ha_norm
)

## rename columns to have the correct name (RECYCLING, SOIL_HEALTH...)
## merge dans indicators_results
## Save le tableau résultat pour

# ==========================================
# 5. Finalisation et Sauvegarde
# ==========================================

# --- 1) Renommer les colonnes normalisées pour plus de clarté ---
composite_indicators_renamed <- composite_indicators %>%
  rename(
    RECYCLING_norm = final_N_total_per_ha_norm,
    INPUT_norm = total_N_legumes_norm,
    SOIL_HEALTH_norm = soc_density_ha_norm,
    BIODIV_norm = biodiv_score_norm,
    ECONOMIC_DIV_norm = eco_div_shannon_raw_norm,
    FAIRNESS_norm = equity_raw_norm,
    CONNECT_norm = connectivity_score_brut_norm,
    SYNERGY_norm = synergy_score_sum_norm
  )


# --- 2) Fusionner les scores composites dans la table de résultats principale ---
# (ae_composite_score et les scores normalisés) à la table 'indicators_results'.
# La jointure se fait sur la colonne commune 'scenario_id'.
indicators_results_final <- indicators_results %>%
  left_join(composite_indicators_renamed, by = "scenario_id")
# boxplot indicators_results_final$ae_composite_score
summary(indicators_results_final$ae_composite_score)
# boxplot all indicators results norm + ae_composite_score on one plot
boxplot(
  indicators_results_final %>%
    select(
      RECYCLING_norm,
      INPUT_norm,
      SOIL_HEALTH_norm,
      BIODIV_norm,
      ECONOMIC_DIV_norm,
      FAIRNESS_norm,
      CONNECT_norm,
      SYNERGY_norm,
      ae_composite_score
    ),
  main = "Boxplot of All Indicators Including AE Composite Score",
  las = 2,
  col = "lightblue"
)

# --- 3) Sauvegarder le tableau de résultats final ---

# Assurez-vous que la librairie 'readr' est chargée pour write_csv
library(readr)

# Nom du fichier de sortie
output_filename <- "AE_Hex_ls/results/v3_final_results_with_composite_scores.csv" #

# Écriture du fichier
write_csv(indicators_results_final, output_filename)


# La fonction glimpse est très pratique pour avoir une vue d'ensemble compacte du résultat
glimpse(indicators_results_final)


hex_grid <- st_read(
  "AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp"
)
hex_grid_terra <- vect(hex_grid)
lulc_rast <- rast("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif")
legend_reclass <- read.csv2(
  "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv"
)

library(sf)
# Remplacez le chemin par le vôtre
path_to_wards <- "data/raw/communal_wards/communal_wards.shp"
ward_boundaries <- st_read(path_to_wards)
