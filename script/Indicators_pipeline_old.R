# RUN_ME.R
# ========
# LANCEZ CE SCRIPT POUR CALCULER TOUS VOS INDICATEURS LULC
# ========

library(dplyr); library(tidyr); library(ggplot2); library(ggridges)
library(forcats); library(GGally); library(cowplot); library(COINr)

# Installation automatique des packages si nécessaire
required_packages <- c("tidyverse", "glue")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages)) {
  cat("📦 Installation packages manquants:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}



# Chargement
library(ggplot2)
library(dplyr)
library(glue)
library(COINr)
library(GGally)

cat("🚀 DÉMARRAGE DU WORKFLOW INDICATEURS LULC\n")
cat("==========================================\n")

rm(list = ls())  # Nettoyage de l'environnement



# ==========================================
# 1. CHARGEMENT PARAMÈTRES
# ==========================================
cat("📋 Chargement paramètres...\n")
source("AE_Hex_ls/script/config_param_AE_hex.R")

# Création dossiers si inexistants
if (!dir.exists("outputs")) dir.create("outputs")

cat("✅ Paramètres chargés - Version:", VERSION, "\n")

# ==========================================
# 2. CHARGEMENT DONNÉES
# ==========================================
cat("📂 Chargement scénarios LULC...\n")

# Vérification fichier existe
if (!file.exists(FILE_PATHS$input_scenarios)) {
  stop("❌ ERREUR: Fichier non trouvé: ", FILE_PATHS$input_scenarios)
}


scenarios_raw <- read.csv("AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v3.csv")
scenarios_raw$scenario_id <- as.character(scenarios_raw$scenario_id)
baseline <- read.csv2("data/processed/baseline_compo.csv")
scenarios_raw <- bind_rows(baseline, scenarios_raw)
str(baseline)
str(scenarios_raw)




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
    RUE = RUE, R = R, wet_months = wet_months,
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
    palette = "YlGn",
    style = "quantile",
    K = 4
  ),
  print_synergy_plot = TRUE
  
)

# Récupération des sorties
scenarios_raw   <- res$scenarios_processed
scenarios_long  <- res$scenarios_long
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




# ------------------------------------------------------------------------------
# A) CALCUL ET INTÉGRATION DU SCORE D'AGROÉCOLOGIE (AE)
# ------------------------------------------------------------------------------

# Charger la fonction AE
source("AE_Hex_ls/script/aggreg_AE_methods.R")












########
# Visualisation des cartes principales des résultats de chaque indicateur 
#########


ae_principles <- c("RECYCLING","SOIL_HEALTH","BIODIV","ECONOMIC_DIV","FAIRNESS","CONNECT","SYNERGY")

required_cols <- c("final_total_N","soc_normalized","biodiv_score",
                   "economic_diversification_score","equity_score",
                   "connectivity_score","synergy_score_norm")
missing_cols <- setdiff(required_cols, names(indicators_results))
if (length(missing_cols) > 0) stop("Colonnes manquantes : ", paste(missing_cols, collapse = ", "))

# Préparer les données
data_complete <- indicators_results %>%
  select(scenario_id,
         final_total_N, soc_normalized, biodiv_score,
         economic_diversification_score, equity_score, connectivity_score,
         synergy_score_norm) %>%
  rename(
    RECYCLING    = final_total_N,
    SOIL_HEALTH  = soc_normalized,
    BIODIV       = biodiv_score,
    ECONOMIC_DIV = economic_diversification_score,
    FAIRNESS     = equity_score,
    CONNECT      = connectivity_score,
    SYNERGY      = synergy_score_norm
  ) %>%
  # Remplacer les NA par 0 d'abord
  mutate(across(all_of(ae_principles), ~ifelse(is.na(.), 0, .)))




# Ajouter tous les indicateurs AE (composites et individuels) à la table des composites
composite_indicators <- data_complete

str(composite_indicators)

# Normalisation min-max sur toutes les colonnes sauf scenario_id
composite_indicators <- composite_indicators %>%
  mutate(across(-scenario_id, ~ {
    min_val <- min(.x, na.rm = TRUE)
    max_val <- max(.x, na.rm = TRUE)
    if (max_val > min_val) {
      (.x - min_val) / (max_val - min_val)
    } else {
      rep(0, length(.x))  # Si toutes les valeurs sont identiques
    }
  }))







#### Visualisation


# Packages
library(sf)
library(landscapemetrics)
library(dplyr)
library(ggplot2)
library(terra)
library(units)
library(exactextractr)

# grid
hex_grid <- st_read("AE_Hex_ls/data/grid_tool/V3_grid_hex_5km2_town_filtered.shp")
hex_grid_terra <- vect(hex_grid)
lulc_rast <- rast("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif")
legend_reclass <- read.csv2("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv")

# plot les hexagones filtréson top of the raster
plot(lulc_rast, 
     col = legend_reclass$color,
     main = "Reclassified LULC with Valid Mask",
     axes = FALSE)
plot(hex_grid_terra, add = TRUE, border = "red", lwd = 1)



scenario_compo <- read.csv("AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v3.csv")



# Fusionner les données basées sur id et scenario_id
merged_data <- merge(hex_grid, scenario_compo, by.x = "id", by.y = "scenario_id", all.x = TRUE)


# Charger les bibliothèques nécessaires
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)

# Fusionner hex_grid et scenario_compo
merged_data <- merge(merged_data, composite_indicators, by.x = "id", by.y = "scenario_id", all.x = TRUE)

library(ggplot2)
library(patchwork)
library(viridis)
library(classInt)
library(tidyverse)







#--------------------------------------------------------------------
# CHARGER LES BIBLIOTHÈQUES NÉCESSAIRES
#--------------------------------------------------------------------
library(sf)
library(ggplot2)
library(patchwork) # Pour wrap_plots
library(viridis)
library(classInt)  # La bibliothèque pour la classification !

#--------------------------------------------------------------------
# ÉTAPE 1 : PRÉPARER LES BORNES (BREAKS) AVEC classIntervals
#--------------------------------------------------------------------

# Liste des indicateurs à visualiser
indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", "FAIRNESS", "CONNECT", "SYNERGY")

# Choisissez ici le nombre de classes que vous souhaitez
n_classes <- 5

# --- LA PARTIE CLÉ ---
# 1. On rassemble toutes les données des indicateurs dans un seul vecteur numérique
all_values <- unlist(st_drop_geometry(merged_data[, indicators]))

# 2. On utilise classIntervals sur l'ensemble de ces données
# Le style "equal" va créer des intervalles de taille égale sur la plage des données (de min(all_values) à max(all_values))
# Comme vos données sont de 0 à 1, le résultat sera le même que seq(0, 1, ...)
intervals <- classIntervals(all_values, n = n_classes, style = "equal")

# 3. On extrait les bornes (breaks)
breaks <- intervals$brks

# On crée les étiquettes (labels) pour la légende
labels <- paste0(round(head(breaks, -1), 2), " - ", round(tail(breaks, -1), 2))

# On affiche pour vérifier :
print("Bornes calculées avec classIntervals :")
print(breaks)
print("Étiquettes pour la légende :")
print(labels)

#--------------------------------------------------------------------
# ÉTAPE 2 : CRÉER LES COLONNES DE CLASSES (INCHANGÉ)
#--------------------------------------------------------------------
# On copie les données pour ne pas modifier l'original
merged_data_classed <- merged_data

# La boucle fonctionne maintenant avec les bornes globales
for(indicator in indicators) {
  new_col_name <- paste0(indicator, "_class")
  merged_data_classed[[new_col_name]] <- cut(
    merged_data_classed[[indicator]],
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )
}

#--------------------------------------------------------------------
# ÉTAPE 3 : GÉNÉRER LES CARTES (INCHANGÉ)
#--------------------------------------------------------------------
plots <- lapply(indicators, function(indicator) {
  class_col <- paste0(indicator, "_class")
  
  ggplot(merged_data_classed) +
    geom_sf(aes_string(fill = class_col), color = NA) + # J'ai ajouté color=NA pour enlever les bordures
    scale_fill_viridis_d(
      option = "viridis", 
      direction = 1, # Inversé pour que jaune = faible, violet = fort
      name = "Classe de score",
      drop = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10)
    ) +
    labs(title = indicator, fill = "")
})

# Arranger les graphiques avec une légende commune
final_plot <- wrap_plots(plots, ncol = 3, nrow = 3, guides = 'collect') & 
  theme(legend.position = "bottom")

print(final_plot)







#--------------------------------------------------------------------
# ÉTAPE 1 : PRÉPARER LES DONNÉES AU FORMAT "LONG"
#--------------------------------------------------------------------
# ggplot préfère le format "long" pour créer des facettes.
# On transforme les colonnes d'indicateurs en une seule colonne "score"
# et une colonne "indicateur" qui dit de quelle variable il s'agit.

long_data <- merged_data %>%
  select(id, all_of(indicators)) %>%
  pivot_longer(
    cols = all_of(indicators),
    names_to = "indicator_name",
    values_to = "score"
  )

#--------------------------------------------------------------------
# ÉTAPE 2 : AJOUTER LES INFORMATIONS DE CLASSE
#--------------------------------------------------------------------
# On applique la même discrétisation que pour les cartes
long_data$score_class <- cut(
  long_data$score,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = TRUE
)


#--------------------------------------------------------------------
# ÉTAPE 3 : CRÉER L'HISTOGRAMME FACETTÉ
#--------------------------------------------------------------------

distribution_plot <- ggplot(long_data, aes(x = score, fill = score_class)) +
  # On utilise geom_histogram. La couleur de remplissage (fill) est déterminée par la classe.
  geom_histogram(bins = 50, alpha = 0.9, color = "white", size=0.1) +
  
  # On crée une facette (un sous-graphique) pour chaque indicateur
  facet_wrap(~ indicator_name, ncol = 1, scales = "free_y") +
  
  # On utilise la même palette de couleurs discrète pour la cohérence
  scale_fill_viridis_d(
    option = "viridis", 
    direction = 1,
    name = "Classe de score",
    drop = FALSE
  ) +
  
  # On ajoute des lignes verticales pour bien marquer les bornes des classes
  geom_vline(xintercept = breaks, linetype = "dashed", color = "black") +
  
  labs(
    title = "Distribution des indicateurs par classe (Intervalles Égaux)",
    x = "Score Normalisé",
    y = "Fréquence"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) # Met le nom des indicateurs en gras

# Afficher le graphique de distribution
print(distribution_plot)







indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", "FAIRNESS", "CONNECT", "SYNERGY")



summary(merged_data_classed)

# --- Calcul du score ---

# Définition des seuils et des labels pour la notation 1-5
score_breaks <- breaks
score_labels <- c(1, 2, 3, 4, 5) # Score de 1 (faible) à 5 (fort)

str(merged_data_classed)
# Le pipe dplyr pour tout calculer
hex_grid_scores <- merged_data_classed %>%
  # Étape A: Reclassifier chaque indicateur en un score de 1 à 5
  # On utilise across() pour appliquer la même opération sur toutes les colonnes finissant par "_class"
  mutate(across(all_of(indicators), 
                ~cut(., breaks = score_breaks, labels = score_labels, include.lowest = TRUE),
                # On crée de nouvelles colonnes avec le suffixe "_score"
                .names = "{.col}_score"
  )) %>%
  
  # Étape B: Convertir les nouvelles colonnes de score (qui sont des "factors") en nombres
  mutate(across(ends_with("_score"), ~as.numeric(as.character(.)))) %>%
  
  # Étape C: Calculer la somme des scores pour chaque hexagone (ligne)
  # rowwise() permet de faire des calculs ligne par ligne
  # c_across() sélectionne les colonnes à sommer
  rowwise() %>%
  mutate(
    AE_total_score = sum(c_across(ends_with("_score")), na.rm = TRUE)
  ) %>%
  ungroup() # Indispensable pour enlever le mode "ligne par ligne"

# --- Afficher le résultat pour vérifier ---
# Vous verrez les colonnes originales, les nouvelles colonnes "_score" et le "AE_total_score"
print(head(hex_grid_scores))

# Pour voir seulement les colonnes de score et le total
print(head(select(st_drop_geometry(hex_grid_scores), ends_with("_score"), AE_total_score)))

# --- Création de la carte ---

p_hotspots_total <- ggplot(hex_grid_scores) +
  geom_sf(aes(fill = AE_total_score), color = "transparent", size = 0) +
  
  # Utilise une échelle de couleur continue pour bien voir le gradient
  scale_fill_viridis_c(
    name = "Agroecology\nscore sum", 
    option = "turbo", # "turbo" est excellent pour distinguer les extrêmes
    direction = -1
  ) +
  
  labs(
    title = "Hotspots and Coldspots (sum equal interval classes)",
    subtitle = ""
  ) +
  
  theme_void() +
  theme(
    legend.position = "right",
    #legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(p_hotspots_total)

