library(dplyr)
library(classInt)




# 1. Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)

# 2. Load data
hex_grid <- st_read("AE_Hex_ls/data/grid_tool/V3_grid_hex_5km2_town_filtered.shp")
indicators_results <- read.csv("AE_Hex_ls/results/final_results_with_composite_scores.csv")
ward_boundaries <- st_read("data/raw/communal_wards/communal_wards.shp") 

output_dir <- "AE_Hex_ls/results/maps/"

# 3. Merge spatial data with results
hex_grid$id <- as.character(hex_grid$id)
# Note: Joining sf object (left) with a dataframe (right)
map_data <- left_join(hex_grid, indicators_results, by = c("id" = "scenario_id"))



# --- Définition des paramètres ---
# La variable source dans votre dataframe
variable_source <- map_data$SYNERGY_norm
# Le nombre de classes souhaité
nombre_de_classes <- 4


# 2. On calcule les seuils avec la méthode "quantile"
seuils <- classIntervals(donnees_propres, n = nombre_de_classes, style = "quantile")$brks

# 3. On s'assure que les seuils sont uniques pour éviter des erreurs
seuils <- unique(seuils)

# --- Application des classes au dataframe ---

# 4. On utilise la fonction cut() pour assigner chaque hexagone à une classe (de 1 à 4)
#    en utilisant les seuils calculés précédemment.
#    - include.lowest=TRUE garantit que la valeur la plus basse est incluse.
#    - labels=FALSE nous retourne directement les numéros de classe (1, 2, 3, 4).
classes_calculees <- cut(variable_source, 
                         breaks = seuils, 
                         include.lowest = TRUE, 
                         labels = FALSE)

# 5. On ajoute cette nouvelle série de classes comme une nouvelle colonne dans le dataframe
map_data$SYNERGY_class <- as.integer(classes_calculees)



# Étape 1 : Préparation des données
# Sélectionner les colonnes des scores catégorisés
es_indicators <- c("RECYCLING_class", "SOIL_HEALTH_class", "BIODIV_class", 
                   "ECONOMIC_DIV_class", "FAIRNESS_class", "CONNECT_class", 
                   "SYNERGY_class")

# Créer un dataframe avec les données nécessaires
data_analysis <- map_data %>%
  select(id, all_of(es_indicators))

# Étape 2 : Redéfinition des catégories
# Fonction pour reclassifier selon la méthode originale
reclassify_scores <- function(score) {
  case_when(
    score == 4 ~ "high",      # Score élevé (équivalent top 25%)
    score %in% c(2, 3) ~ "moderate",  # Score modéré (50% moyens)
    score == 1 ~ "low"       # Score faible (25% bas)
    # TRUE ~ "moderate"
  )
}

# Appliquer la reclassification
data_reclassified <- data_analysis %>%
  mutate(across(all_of(es_indicators), reclassify_scores, .names = "{.col}_reclass"))




# Étape 3 : Calcul des relations par paires
# Fonction pour déterminer le type de relation entre deux indicateurs
determine_relationship <- function(score1, score2) {
  if (score1 == "high" & score2 == "high") {
    return("synergy")
  } else if (score1 == "low" & score2 == "low") {
    return("loss")
  } else if ((score1 == "high" & score2 == "low") | (score1 == "low" & score2 == "high")) {
    return("trade_off")
  } else {
    return("no_relationship")
  }
}

# Obtenir toutes les combinaisons possibles de paires d'indicateurs
reclassified_cols <- paste0(es_indicators, "_reclass")
combinations <- combn(reclassified_cols, 2, simplify = FALSE)

# Initialiser les colonnes de comptage
data_results <- data_reclassified %>%
  mutate(
    synergy_count = 0,
    trade_off_count = 0,
    loss_count = 0,
    no_relationship_count = 0
  )

# Calculer les relations pour chaque paire et chaque observation
for (i in 1:nrow(data_results)) {
  synergy_total <- 0
  trade_off_total <- 0
  loss_total <- 0
  no_relationship_total <- 0
  
  for (combo in combinations) {
    col1 <- combo[1]
    col2 <- combo[2]
    
    score1 <- data_results[i, col1, drop = TRUE]
    score2 <- data_results[i, col2, drop = TRUE]
    
    relationship <- determine_relationship(score1, score2)
    
    if (relationship == "synergy") {
      synergy_total <- synergy_total + 1
    } else if (relationship == "trade_off") {
      trade_off_total <- trade_off_total + 1
    } else if (relationship == "loss") {
      loss_total <- loss_total + 1
    } else {
      no_relationship_total <- no_relationship_total + 1
    }
  }
  
  data_results[i, "synergy_count"] <- synergy_total
  data_results[i, "trade_off_count"] <- trade_off_total
  data_results[i, "loss_count"] <- loss_total
  data_results[i, "no_relationship_count"] <- no_relationship_total
}

# Étape 4 : Calcul du nombre total de paires possibles (pour vérification)
total_pairs <- length(combinations)
print(paste("Nombre total de paires analysées :", total_pairs))

summary(data_results)



# Charger la librairie nécessaire pour Jenks
library(classInt)

# Étape 5 : Classification des intensités avec Jenks natural breaks
# Fonction corrigée pour la classification avec gestion des valeurs nulles
classify_intensity_corrected <- function(values) {
  # Séparer les valeurs nulles des non-nulles
  zero_indices <- which(values == 0)
  non_zero_values <- values[values > 0]
  
  # Si toutes les valeurs sont nulles
  if (length(non_zero_values) == 0) {
    return(factor(rep("none", length(values)), levels = c("none", "weak", "moderate", "strong")))
  }
  
  # Initialiser le résultat
  result <- character(length(values))
  
  # Assigner "none" aux valeurs nulles
  result[zero_indices] <- "none"
  
  # Classifier les valeurs non-nulles
  if (length(unique(non_zero_values)) == 1) {
    # Si toutes les valeurs non-nulles sont identiques
    result[values > 0] <- "weak"
  } else if (length(unique(non_zero_values)) == 2) {
    # Si seulement 2 valeurs uniques non-nulles
    median_val <- median(non_zero_values)
    result[values > 0] <- ifelse(values[values > 0] <= median_val, "weak", "strong")
  } else {
    # Classification Jenks pour les valeurs non-nulles (au moins 3 valeurs uniques)
    breaks <- classIntervals(non_zero_values, n = 3, style = "jenks")$brks
    # S'assurer que les breaks sont uniques
    breaks[1] <- breaks[1] - 0.001  # Ajuster légèrement le premier break
    
    classified <- cut(values[values > 0], breaks = breaks, 
                      labels = c("weak", "moderate", "strong"), 
                      include.lowest = TRUE, right = TRUE)
    result[values > 0] <- as.character(classified)
  }
  
  return(factor(result, levels = c("none", "weak", "moderate", "strong")))
}

# Appliquer la classification corrigée
data_results <- data_results %>%
  mutate(
    synergy_intensity = classify_intensity_corrected(synergy_count),
    trade_off_intensity = classify_intensity_corrected(trade_off_count),
    loss_intensity = classify_intensity_corrected(loss_count)
  )

str(data_results)


# Vérifier les résultats
print("Distribution des synergy_intensity:")
table(data_results$synergy_intensity)

print("Distribution des trade_off_intensity:")
table(data_results$trade_off_intensity)

print("Distribution des loss_intensity:")
table(data_results$loss_intensity)

# Vérifier quelques exemples
head(data_results %>% select(id, synergy_count, synergy_intensity, 
                             trade_off_count, trade_off_intensity, 
                             loss_count, loss_intensity), 10)



summary(data_results$synergy_count)
summary(data_results$trade_off_count)
summary(data_results$loss_count)















# Étape 6 : Identification de la relation dominante
data_results <- data_results %>%
  rowwise() %>%
  mutate(
    dominant_relationship = case_when(
      synergy_count >= trade_off_count & synergy_count >= loss_count ~ "synergy_dominant",
      trade_off_count >= synergy_count & trade_off_count >= loss_count ~ "trade_off_dominant",
      loss_count >= synergy_count & loss_count >= trade_off_count ~ "loss_dominant",
      TRUE ~ "mixed"
    )
  ) %>%
  ungroup()

# Résultats finaux
final_results <- data_results %>%
  select(id, 
         synergy_count, trade_off_count, loss_count, no_relationship_count,
         synergy_intensity, trade_off_intensity, loss_intensity,
         dominant_relationship)

# Afficher un résumé des résultats
print("Résumé des relations :")
print(summary(final_results[, c("synergy_count", "trade_off_count", "loss_count")]))

print("Distribution des relations dominantes :")
print(table(final_results$dominant_relationship))

print("Distribution des intensités de synergies :")
print(table(final_results$synergy_intensity))

print("Distribution des intensités de trade-offs :")
print(table(final_results$trade_off_intensity))

print("Distribution des intensités de pertes :")
print(table(final_results$loss_intensity))

# Visualiser les premiers résultats
head(final_results, 10)


final_results$dominant_relationship <- as.factor(final_results$dominant_relationship)
summary(final_results$dominant_relationship)

sy_to_lo <- final_results


str(sy_to_lo)


library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)

# Étape 1 : Créer la table sy_to_lo avec toutes les variables d'intensité
sy_to_lo <- data_results %>%
  select(scenario_id, synergy_count, trade_off_count, loss_count,
         synergy_intensity, trade_off_intensity, loss_intensity, dominant_relationship)

# Étape 2 : Left join avec agg_ae
agg_ae_complete <- agg_ae %>%
  left_join(sy_to_lo, by = "scenario_id")

# Étape 3 : Left join avec hex_grid (attention : id dans hex_grid = scenario_id)
hex_grid$id <- as.character(hex_grid$id)
hex_grid_complete <- hex_grid %>%
  left_join(agg_ae_complete, by = c("id" = "scenario_id"))


summary(hex_grid_complete$AE_score_sum_raw)
hex_grid_complete$dominant_relationship <- as.factor(hex_grid_complete$dominant_relationship)
summary(hex_grid_complete)



# 1. CARTE DES HOTSPOTS/COLDSPOTS AE
p_hotspot <- ggplot(map_data) +
  geom_sf(aes(fill = AE_score_sum_raw), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "AE Score", option = "turbo", direction = -1,  na.value = "grey90") +
  theme_void() +
  labs(title = "Hotspots and coldspots of AE indicators scores") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

print(p_hotspot)




# 2. Définir les classes pour la discrétisation
# Nous allons d'abord trouver les "breaks" (seuils) optimaux.

# Définissez le nombre de classes que vous souhaitez
nombre_de_classes <- 3 # Vous pouvez ajuster ce chiffre

# Utilisez classIntervals pour trouver les meilleurs seuils.
# La méthode "jenks" (Natural Breaks) est excellente pour la cartographie
# car elle regroupe les valeurs similaires et maximise les différences entre les classes.
# D'autres styles existent : "quantile", "equal", "sd"...
breaks_jenks <- classIntervals(hex_grid_complete$AE_score_sum_raw, 
                               n = nombre_de_classes, 
                               style = "jenks")

# 3. Créer la nouvelle variable discrète dans votre dataframe
hex_grid_complete$AE_score_discrete <- cut(hex_grid_complete$AE_score_sum_raw,
                                           breaks = breaks_jenks$brks,
                                           include.lowest = TRUE, # Très important pour inclure la valeur la plus basse
                                           labels = NULL) # Laissez R générer les labels (ex: "(10,20]")

# 4. Créer le graphique avec la nouvelle variable et l'échelle discrète
p_hotspot_discrete <- ggplot(hex_grid_complete) +
  # Utilisez la nouvelle variable discrète pour l'esthétique "fill"
  geom_sf(aes(fill = AE_score_discrete), color = "white", size = 0.1) +
  
  # Utilisez une échelle de couleurs DISCRÈTE (notez le "_d" à la fin)
  scale_fill_viridis_d(
    name = "AE Score", 
    option = "turbo", 
    direction = -1,  
    na.value = "grey90",
    drop = FALSE # S'assure que toutes les classes sont affichées dans la légende
  ) +
  
  theme_void() +
  labs(title = "Hotspots and Coldspots of AE Indicators Scores (Discretized)") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    # Guide pour la légende avec des barres de couleur au lieu de carrés
    guides(fill = guide_legend(nrow = 1, label.position = "bottom")) 
  )

print(p_hotspot_discrete)






# 2. COMBINAISON DE 4 CARTES - VERSION SIMPLIFIÉE

# Préparer les données
hex_grid_complete <- hex_grid_complete %>%
  mutate(
    # S'assurer que les variables sont des facteurs dans le bon ordre
    synergy_intensity = factor(synergy_intensity, levels = c("none", "weak", "moderate", "strong")),
    trade_off_intensity = factor(trade_off_intensity, levels = c("none", "weak", "moderate", "strong")),
    loss_intensity = factor(loss_intensity, levels = c("none", "weak", "moderate", "strong")),
    
    # Calculer la relation dominante (comme avant)
    syn_num = case_when(
      synergy_intensity == "strong" ~ 3,
      synergy_intensity == "moderate" ~ 2,
      synergy_intensity == "weak" ~ 1,
      TRUE ~ 0
    ),
    trade_num = case_when(
      trade_off_intensity == "strong" ~ 3,
      trade_off_intensity == "moderate" ~ 2,
      trade_off_intensity == "weak" ~ 1,
      TRUE ~ 0
    ),
    loss_num = case_when(
      loss_intensity == "strong" ~ 3,
      loss_intensity == "moderate" ~ 2,
      loss_intensity == "weak" ~ 1,
      TRUE ~ 0
    ),
    
    dominant_relationship = case_when(
      syn_num == 0 & trade_num == 0 & loss_num == 0 ~ "No significant relationship",
      syn_num > trade_num & syn_num > loss_num ~ "Synergies",
      trade_num > syn_num & trade_num > loss_num ~ "Trade-offs",
      loss_num > syn_num & loss_num > trade_num ~ "Losses",
      syn_num == trade_num & syn_num > loss_num ~ "Mixed (Syn-TO)",
      syn_num == loss_num & syn_num > trade_num ~ "Mixed (Syn-Loss)",
      trade_num == loss_num & trade_num > syn_num ~ "Mixed (TO-Loss)",
      TRUE ~ "Balanced"
    ),
    
    dominant_relationship = factor(dominant_relationship)
  )

# Couleurs plus discriminantes pour les intensités
intensity_colors <- c(
  "none" = "transparent",  # Transparent pour none
  "weak" = "#B7E6A5FF",      # Jaune très pâle
  "moderate" = "#46AEA0FF",  # Orange moyen
  "strong" = "#045275FF"     # Rouge foncé
)

# Couleurs plus contrastées pour les relations dominantes
dominant_colors <- c(
  "Synergies" = "#00496FFF",        # Bleu vif
  "Trade-offs" = "#EDD746FF",       # Orange vif  
  "Losses" = "#DD4124FF",           # Rouge vif
  "No significant relationship" = "white",     # Blanc comme demandé
  "Mixed (Syn-TO)" = "#0F85A0FF",    # Vert vif
  "Mixed (Syn-Loss)" = "#6a3d9a",  # Violet
  "Mixed (TO-Loss)" = "#ED8B00FF",   # Jaune vif
  "Balanced" = "#b15928"           # Marron
)

# Cartes avec les nouvelles couleurs
p_syn <- ggplot(hex_grid_complete) +
  geom_sf(aes(fill = synergy_intensity), color = "white", size = 0.1) +
  scale_fill_manual(values = intensity_colors, name = "Intensity") +
  theme_void() +
  labs(title = "Synergies") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p_trade <- ggplot(hex_grid_complete) +
  geom_sf(aes(fill = trade_off_intensity), color = "white", size = 0.1) +
  scale_fill_manual(values = intensity_colors, name = "Intensity") +
  theme_void() +
  labs(title = "Trade-offs") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p_loss <- ggplot(hex_grid_complete) +
  geom_sf(aes(fill = loss_intensity), color = "white", size = 0.1) +
  scale_fill_manual(values = intensity_colors, name = "Intensity") +
  theme_void() +
  labs(title = "Losses") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p_dominant <- ggplot(hex_grid_complete) +
  geom_sf(aes(fill = dominant_relationship), color = "white", size = 0.1) +
  scale_fill_manual(values = dominant_colors, name = "Dominant relationship") +
  theme_void() +
  labs(title = "Dominant relationship") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Vérifier ce que représente "No significant relationship"
cat("Analyse des 'No significant relationship':\n")
no_rel_check <- hex_grid_complete %>%
  st_drop_geometry() %>%
  filter(dominant_relationship == "No significant relationship") %>%
  select(synergy_intensity, trade_off_intensity, loss_intensity, syn_num, trade_num, loss_num)

cat("Nombre de 'No significant relationship':", nrow(no_rel_check), "\n")
if(nrow(no_rel_check) > 0) {
  cat("Exemple des premières lignes:\n")
  print(head(no_rel_check))
}

# Peut-être voulez-vous exclure les "No relationship" de l'analyse ?
# Ou les renommer en "No significant relationship" ?

# Alternative : modifier la logique pour exclure les cas où tout est "none"
hex_grid_complete_alt <- hex_grid_complete %>%
  mutate(
    dominant_relationship_clean = case_when(
      syn_num == 0 & trade_num == 0 & loss_num == 0 ~ "No significant relationship",  # Changé ici
      syn_num > trade_num & syn_num > loss_num ~ "Synergies",
      trade_num > syn_num & trade_num > loss_num ~ "Trade-offs",
      loss_num > syn_num & loss_num > trade_num ~ "Losses",
      syn_num == trade_num & syn_num > loss_num ~ "Mixed (Syn-TO)",
      syn_num == loss_num & syn_num > trade_num ~ "Mixed (Syn-Loss)",
      trade_num == loss_num & trade_num > syn_num ~ "Mixed (TO-Loss)",
      TRUE ~ "Balanced"
    ),
    dominant_relationship_clean = factor(dominant_relationship_clean)
  )

# Recalculer les proportions sans les NA
prop_table_clean <- table(hex_grid_complete_alt$dominant_relationship_clean)  # Supprimé useNA = "no"
prop_perc_clean <- round(100 * prop_table_clean / sum(prop_table_clean), 1)

# Couleurs sans "No relationship"
dominant_colors_clean <- dominant_colors  # Ne rien supprimer !

# Nouvelle carte dominante sans "No relationship"
p_dominant_clean <- ggplot(hex_grid_complete_alt) +
  geom_sf(aes(fill = dominant_relationship_clean), color = "white", size = 0.1) +
  scale_fill_manual(values = dominant_colors_clean, name = "Dominant relationship") +
  theme_void() +
  labs(title = "Dominant relationship") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Créer les légendes
legend_intensity <- ggplot(hex_grid_complete) +
  geom_sf(aes(fill = synergy_intensity), color = "white", size = 0.1) +
  scale_fill_manual(values = intensity_colors, name = "Intensity") +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, override.aes = list(color = "black", size = 0.5)))

# Légende dominante avec proportions propres
if(length(prop_perc_clean) > 0) {
  dominant_labels_clean <- paste0(names(prop_perc_clean), " (", prop_perc_clean, "%)")
  names(dominant_labels_clean) <- names(prop_perc_clean)
  
  legend_dominant_clean <- ggplot(hex_grid_complete_alt) +
    geom_sf(aes(fill = dominant_relationship_clean), color = "white", size = 0.1) +
    scale_fill_manual(values = dominant_colors_clean, 
                      name = "Dominant relationship",
                      labels = dominant_labels_clean,
                      na.value = "white") +
    theme_void() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, override.aes = list(color = "black", size = 0.5)))
}

# Assembler
leg_intensity <- get_legend(legend_intensity)
leg_dominant_clean <- get_legend(legend_dominant_clean)

main_panel <- plot_grid(p_syn, p_trade, p_loss, p_dominant_clean, 
                        ncol = 2, nrow = 2)

legend_panel <- plot_grid(leg_intensity, leg_dominant_clean, 
                          ncol = 2, rel_widths = c(1, 2))

final_plot <- plot_grid(main_panel, legend_panel, 
                        ncol = 1, rel_heights = c(4, 1))



print(final_plot)

# Afficher les proportions
cat("Proportions des relations dominantes:\n")
for(i in 1:length(prop_perc_clean)) {
  cat(names(prop_perc_clean)[i], ":", prop_perc_clean[i], "%\n")
}







library(ggplot2)
library(dplyr)
library(reshape2)
library(viridis)

# Liste des indicateurs AE
ae_indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", "FAIRNESS", "CONNECT", "SYNERGY")

# Fonction pour calculer les relations entre toutes les paires d'indicateurs
calculate_pairwise_relationships <- function(data, indicators) {
  n_indicators <- length(indicators)
  
  # Matrices pour stocker les pourcentages
  synergy_matrix <- matrix(NA, nrow = n_indicators, ncol = n_indicators)
  tradeoff_matrix <- matrix(NA, nrow = n_indicators, ncol = n_indicators)
  loss_matrix <- matrix(NA, nrow = n_indicators, ncol = n_indicators)
  
  # Noms des lignes et colonnes
  rownames(synergy_matrix) <- rownames(tradeoff_matrix) <- rownames(loss_matrix) <- indicators
  colnames(synergy_matrix) <- colnames(tradeoff_matrix) <- colnames(loss_matrix) <- indicators
  
  # Calculer SEULEMENT pour la partie triangulaire supérieure
  for(i in 1:(n_indicators-1)) {
    for(j in (i+1):n_indicators) {
      
      indicator1 <- paste0(indicators[i], "_class")
      indicator2 <- paste0(indicators[j], "_class")
      
      # Extraire les valeurs pour cette paire
      val1 <- data[[indicator1]]
      val2 <- data[[indicator2]]
      
      # Supprimer les NA
      valid_idx <- !is.na(val1) & !is.na(val2)
      val1_clean <- val1[valid_idx]
      val2_clean <- val2[valid_idx]
      
      if(length(val1_clean) == 0) next
      
      # Calculer les différences
      diff_vals <- val1_clean - val2_clean
      
      # Compter les types de relations
      synergies <- sum(diff_vals == 0 & val1_clean > 2) # Même classe élevée
      tradeoffs <- sum(abs(diff_vals) >= 2) # Différence importante
      losses <- sum(diff_vals == 0 & val1_clean <= 1) # Même classe faible
      
      # Calculer les pourcentages
      synergy_pct <- (synergies / length(val1_clean)) * 100
      tradeoff_pct <- (tradeoffs / length(val1_clean)) * 100
      loss_pct <- (losses / length(val1_clean)) * 100
      
      # Remplir SEULEMENT la partie triangulaire supérieure
      synergy_matrix[i, j] <- synergy_pct
      tradeoff_matrix[i, j] <- tradeoff_pct
      loss_matrix[i, j] <- loss_pct
    }
  }
  
  return(list(
    synergy = synergy_matrix,
    tradeoff = tradeoff_matrix,
    loss = loss_matrix
  ))
}

# Calculer les matrices
relationship_matrices <- calculate_pairwise_relationships(hex_grid_complete, ae_indicators)

# Fonction pour créer une heatmap triangulaire
create_relationship_heatmap <- function(matrix_data, title, color_scale = "viridis") {
  # Convertir la matrice en format long pour ggplot
  melted_data <- melt(matrix_data)
  colnames(melted_data) <- c("Indicator1", "Indicator2", "Percentage")
  
  # Supprimer les valeurs NA (partie triangulaire inférieure et diagonale)
  melted_data <- melted_data[!is.na(melted_data$Percentage), ]
  
  # Créer la heatmap
  p <- ggplot(melted_data, aes(x = Indicator1, y = Indicator2, fill = Percentage)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(Percentage, 1)), 
              color = ifelse(melted_data$Percentage > max(melted_data$Percentage)/2, "white", "black"),
              size = 3) +
    scale_fill_viridis_c(name = "Percent of pairs", option = color_scale, direction = -1) +
    labs(title = title,
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 12)
    ) +
    coord_fixed(ratio = 1)
  
  return(p)
}

# Créer les trois heatmaps
heatmap_synergies <- create_relationship_heatmap(
  relationship_matrices$synergy, 
  "a) Synergies between AE Indicators", 
  "cividis"
)

heatmap_tradeoffs <- create_relationship_heatmap(
  relationship_matrices$tradeoff, 
  "b) Trade-offs between AE Indicators", 
  "cividis"
)

heatmap_losses <- create_relationship_heatmap(
  relationship_matrices$loss, 
  "c) Losses between AE Indicators", 
  "cividis"
)

# Afficher les heatmaps
print(heatmap_synergies)
print(heatmap_tradeoffs)
print(heatmap_losses)

# Optionnel : Combiner les trois heatmaps en une seule figure
library(gridExtra)
combined_heatmaps <- grid.arrange(heatmap_synergies, heatmap_tradeoffs, heatmap_losses, 
                                  ncol = 2, nrow = 2)

# Afficher les statistiques des matrices
cat("Statistiques des relations :\n")
cat("Synergies - Min:", round(min(relationship_matrices$synergy, na.rm = TRUE), 2), 
    "% | Max:", round(max(relationship_matrices$synergy, na.rm = TRUE), 2), "%\n")
cat("Trade-offs - Min:", round(min(relationship_matrices$tradeoff, na.rm = TRUE), 2), 
    "% | Max:", round(max(relationship_matrices$tradeoff, na.rm = TRUE), 2), "%\n")
cat("Losses - Min:", round(min(relationship_matrices$loss, na.rm = TRUE), 2), 
    "% | Max:", round(max(relationship_matrices$loss, na.rm = TRUE), 2), "%\n")

