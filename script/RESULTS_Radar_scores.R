library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(gridExtra)
library(viridis)


legend_reclass <- read.csv2("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv")


ae_indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", 
                   "FAIRNESS", "CONNECT", "SYNERGY")

# 1. Identifier les paysages représentatifs
get_representative_landscapes <- function(agg_ae) {
  sorted_data <- agg_ae[order(agg_ae$AE_Percentiles, decreasing = TRUE), ]
  
  high_ae <- sorted_data[1, ]
  low_ae <- sorted_data[nrow(sorted_data), ]
  median_ae <- sorted_data[round(nrow(sorted_data)/2), ]
  
  return(list(
    high = high_ae,
    median = median_ae,
    low = low_ae
  ))
}





# 2. GRAPHIQUE DE COMPOSITION AVEC ORDRE SPÉCIFIQUE
create_ordered_composition_plot <- function(representative_data, scenario_compo) {
  
  scenarios <- c(representative_data$high$scenario_id, 
                 representative_data$median$scenario_id, 
                 representative_data$low$scenario_id)
  
  labels <- c("High AE Score", "Medium AE Score", "Low AE Score")
  ae_scores <- c(representative_data$high$AE_Percentiles,
                 representative_data$median$AE_Percentiles,
                 representative_data$low$AE_Percentiles)
  
  combined_data <- data.frame()
  
  # Ordre spécifique des types d'occupation du sol
  land_use_order <- c("dense_woodland", "open_woodland", 
                      "grassland", "cropland", "urban", "horticulture" , "wetland_grassland", "mineral_bare_soil", "water")
  
  for(i in 1:3) {
    comp_data <- scenario_compo[scenario_compo$scenario_id == as.numeric(scenarios[i]), ]
    
    # CRÉER comp_long EN UTILISANT LES NOMS DU CSV
    comp_long <- data.frame(
      scenario_label = paste(labels[i], "\n(Score:", round(ae_scores[i], 1), ")"),
      land_use = legend_reclass$class_name,
      percentage = c(comp_data$dense_woodland, comp_data$cropland, comp_data$open_woodland,
                     comp_data$mineral_bare_soil, comp_data$urban, comp_data$water,
                     comp_data$horticulture, comp_data$grassland, comp_data$wetland_grassland) * 100,
      order = i
    )
    
    # Supprimer les valeurs très faibles pour la lisibilité
    comp_long$percentage[comp_long$percentage < 0.1] <- 0
    combined_data <- rbind(combined_data, comp_long)
  }
  
  # CRÉER LE MAPPING DES COULEURS À PARTIR DU CSV
  color_mapping <- setNames(legend_reclass$color, legend_reclass$class_name)
  
  # Factoriser avec l'ordre du CSV
  combined_data$land_use <- factor(combined_data$land_use, levels = land_use_order)
  combined_data$scenario_label <- factor(combined_data$scenario_label, 
                                         levels = unique(combined_data$scenario_label[order(combined_data$order)]))
  
  p <- ggplot(combined_data, aes(x = scenario_label, y = percentage, fill = land_use)) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(values = color_mapping, name = "Land Use Type") +
    labs(title = "Landscape Composition Comparison",
         x = "AE Score Category",
         y = "Percentage (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 11),
      axis.title = element_text(size = 13),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 1))
  
  return(p)
}






# 3. SPIDER PLOT AMÉLIORÉ avec légende visible
create_enhanced_spider_plot <- function(scenario_id, agg_ae, title_suffix) {
  
  ae_data <- agg_ae[agg_ae$scenario_id == scenario_id, ]
  
  ae_indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", 
                     "FAIRNESS", "CONNECT", "SYNERGY")
  
  # Préparer les données pour le spider plot
  values <- as.numeric(ae_data[ae_indicators])
  
  # Créer un dataframe avec les données + les lignes d'échelle
  spider_data <- data.frame(
    indicator = factor(ae_indicators, levels = ae_indicators),
    value = values,
    color_value = values  # Pour la couleur
  )
  
  # Créer les lignes d'échelle (grilles concentriques)
  scale_lines <- expand.grid(
    indicator = factor(ae_indicators, levels = ae_indicators),
    scale_value = c(20, 40, 60, 80, 100)
  )
  
  # Créer les labels d'échelle
  scale_labels <- data.frame(
    indicator = factor(ae_indicators[1], levels = ae_indicators),
    scale_value = c(20, 40, 60, 80, 100),
    label = c("20", "40", "60", "80", "100")
  )
  
  # Créer le graphique
  p <- ggplot() +
    # Lignes d'échelle (grilles concentriques)
    geom_col(data = scale_lines, 
             aes(x = indicator, y = scale_value), 
             fill = "grey90", alpha = 0.3, width = 1) +
    
    # Données principales avec couleur viridis
    geom_col(data = spider_data, 
             aes(x = indicator, y = value, fill = color_value), 
             alpha = 0.8, width = 0.8) +
    
    # Labels d'échelle
    geom_text(data = scale_labels,
              aes(x = indicator, y = scale_value, label = label),
              size = 3, color = "grey60", hjust = 1.2) +
    
    # Échelle de couleur viridis
    scale_fill_viridis_c(name = "AE Value", option = "viridis", 
                         breaks = c(0, 25, 50, 75, 100)) +
    
    # Coordonnées polaires
    coord_polar() +
    
    # Échelle Y fixe
    ylim(0, 100) +
    
    # Titres et thème
    labs(title = paste("AE Indicators", title_suffix),
         subtitle = paste("AE Score:", round(ae_data$AE_Percentiles, 2))) +
    
    theme_minimal() +
    theme(
      # Supprimer les éléments d'axe Y (remplacés par les labels)
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      # Styliser les labels des indicateurs
      axis.text.x = element_text(size = 10, face = "bold"),
      
      # Titres
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      
      # Légende couleur VISIBLE
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      
      # Supprimer les noms d'axes
      axis.title = element_blank(),
      
      # Grille
      panel.grid.major.x = element_line(color = "grey80", size = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# 4. CRÉER LES SPIDER PLOTS SÉPARÉS
create_spider_plots_separate <- function(representative_data, agg_ae) {
  
  # Créer les 3 spider plots avec légendes
  spider_high <- create_enhanced_spider_plot(representative_data$high$scenario_id, agg_ae, "(High)")
  spider_median <- create_enhanced_spider_plot(representative_data$median$scenario_id, agg_ae, "(Medium)")
  spider_low <- create_enhanced_spider_plot(representative_data$low$scenario_id, agg_ae, "(Low)")
  
  # Arranger les 3 graphiques avec titre général
  combined_spiders <- grid.arrange(
    spider_high, spider_median, spider_low, 
    ncol = 3,
    top = textGrob("AE Indicators Comparison: High, Medium, and Low Scoring Landscapes", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  return(combined_spiders)
}

# 5. FONCTIONS PRINCIPALES SÉPARÉES
create_landscape_composition_only <- function(agg_ae, scenario_compo) {
  
  # Identifier les paysages représentatifs
  representative <- get_representative_landscapes(agg_ae)
  
  # Créer le graphique empilé ordonné
  comp_plot <- create_ordered_composition_plot(representative, scenario_compo)
  
  return(list(
    plot = comp_plot,
    data = representative
  ))
}

create_spider_charts_only <- function(agg_ae, scenario_compo) {
  
  # Identifier les paysages représentatifs
  representative <- get_representative_landscapes(agg_ae)
  
  # Créer les spider plots
  spider_plots <- create_spider_plots_separate(representative, agg_ae)
  
  return(list(
    plot = spider_plots,
    data = representative
  ))
}




# 6. EXÉCUTION ET AFFICHAGE SÉPARÉS

# Graphique de composition des paysages
landscape_composition <- create_landscape_composition_only(agg_ae, scenario_compo)
print("=== LANDSCAPE COMPOSITION PLOT ===")
print(landscape_composition$plot)

# Spider charts des indicateurs AE
spider_charts <- create_spider_charts_only(agg_ae, scenario_compo)
print("\n=== SPIDER CHARTS ===")
print(spider_charts$plot)

# Afficher les informations détaillées
representative <- landscape_composition$data
cat("\n=== LANDSCAPE SHOWCASE SUMMARY ===\n\n")

cat("HIGH AE SCORE LANDSCAPE:\n")
cat("Scenario ID:", representative$high$scenario_id, "\n")
cat("AE Score:", round(representative$high$AE_Percentiles, 2), "\n")
cat("Rank:", representative$high$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")

cat("MEDIUM AE SCORE LANDSCAPE:\n")
cat("Scenario ID:", representative$median$scenario_id, "\n")
cat("AE Score:", round(representative$median$AE_Percentiles, 2), "\n")
cat("Rank:", representative$median$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")

cat("LOW AE SCORE LANDSCAPE:\n")
cat("Scenario ID:", representative$low$scenario_id, "\n")
cat("AE Score:", round(representative$low$AE_Percentiles, 2), "\n")
cat("Rank:", representative$low$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")

# Sauvegarder séparément









library(dplyr)
library(ggplot2)
library(viridis)
library(grid)

# 1. Identifier les paysages représentatifs
get_representative_landscapes <- function(agg_ae) {
  sorted_data <- agg_ae[order(agg_ae$AE_Percentiles, decreasing = TRUE), ]
  
  high_ae <- sorted_data[1, ]
  low_ae <- sorted_data[nrow(sorted_data), ]
  median_ae <- sorted_data[round(nrow(sorted_data)/2), ]
  
  return(list(
    high = high_ae,
    median = median_ae,
    low = low_ae
  ))
}

# 2. Créer un spider plot individuel avec labels d'échelle personnalisés
create_individual_spider_plot <- function(data, title, score) {
  
  # Colonnes des indicateurs AE
  ae_indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "CONNECT", 
                     "ECONOMIC_DIV", "FAIRNESS", "SYNERGY")
  
  # Préparer les données pour le spider plot
  spider_data <- data.frame(
    indicator = factor(ae_indicators, levels = ae_indicators),
    value = as.numeric(data[ae_indicators]),
    color_value = as.numeric(data[ae_indicators])
  )
  
  # Remplacer les NA par 0
  spider_data$value[is.na(spider_data$value)] <- 0
  spider_data$color_value[is.na(spider_data$color_value)] <- 0
  
  # Créer les lignes d'échelle (grilles concentriques)
  scale_lines <- expand.grid(
    indicator = factor(ae_indicators, levels = ae_indicators),
    scale_value = c(20, 40, 60, 80, 100)
  )
  
  # Créer les labels d'échelle - positionnés sur le premier indicateur
  scale_labels <- data.frame(
    indicator = factor(ae_indicators[1], levels = ae_indicators),
    scale_value = c(20, 40, 60, 80, 100),
    label = c("20", "40", "60", "80", "100")
  )
  
  # Créer le graphique polaire avec labels d'échelle
  p <- ggplot() +
    # Lignes d'échelle (grilles concentriques)
    geom_col(data = scale_lines, 
             aes(x = indicator, y = scale_value), 
             fill = "grey90", alpha = 0.3, width = 1) +
    
    # Données principales avec couleur viridis
    geom_col(data = spider_data, 
             aes(x = indicator, y = value, fill = color_value), 
             alpha = 0.8, width = 0.8, color = "white", size = 0.3) +
    
    # Labels d'échelle personnalisés
    geom_text(data = scale_labels,
              aes(x = indicator, y = scale_value, label = label),
              size = 3.5, color = "grey50", hjust = 1.3, fontface = "bold") +
    
    # Échelle de couleur viridis plasma
    scale_fill_viridis_c(name = "Indicator\nValue", option = "viridis", 
                         limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    
    # Coordonnées polaires
    coord_polar() +
    
    # Limites Y fixées
    ylim(0, 100) +
    
    # Titres
    labs(title = paste("AE Indicators", title, "\nAE Score:", round(score, 1))) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      
      # Supprimer les éléments d'axe Y (remplacés par les labels personnalisés)
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      
      # Styliser les labels des indicateurs
      axis.text.x = element_text(size = 11, face = "bold", color = "black"),
      
      # Grilles
      panel.grid.major.x = element_line(color = "grey80", size = 0.4),
      panel.grid.major.y = element_line(color = "grey70", size = 0.3),
      panel.grid.minor = element_blank(),
      
      # Légende visible
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      
      # Marges
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}

# 3. Créer les trois spider plots individuels
create_spider_plots_individual <- function(agg_ae) {
  
  # Identifier les paysages représentatifs
  representative <- get_representative_landscapes(agg_ae)
  
  # Créer chaque spider plot individuellement
  spider_high <- create_individual_spider_plot(
    representative$high, 
    "(High Performance)", 
    representative$high$AE_Percentiles
  )
  
  spider_median <- create_individual_spider_plot(
    representative$median, 
    "(Medium Performance)", 
    representative$median$AE_Percentiles
  )
  
  spider_low <- create_individual_spider_plot(
    representative$low, 
    "(Low Performance)", 
    representative$low$AE_Percentiles
  )
  
  return(list(
    high = spider_high,
    medium = spider_median,
    low = spider_low,
    data = representative
  ))
}

# 4. Exécution et création des spider plots individuels
spider_plots <- create_spider_plots_individual(agg_ae)

# 5. Afficher chaque spider plot séparément
print("=== HIGH PERFORMANCE SPIDER PLOT ===")
print(spider_plots$high)

print("\n=== MEDIUM PERFORMANCE SPIDER PLOT ===")
print(spider_plots$medium)

print("\n=== LOW PERFORMANCE SPIDER PLOT ===")
print(spider_plots$low)

# 6. Afficher les informations détaillées
representative <- spider_plots$data
cat("\n=== INDIVIDUAL SPIDER PLOTS SUMMARY ===\n\n")

cat("HIGH PERFORMANCE LANDSCAPE:\n")
cat("Scenario ID:", representative$high$scenario_id, "\n")
cat("AE Score:", round(representative$high$AE_Percentiles, 2), "\n")
cat("Rank:", representative$high$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")

cat("MEDIUM PERFORMANCE LANDSCAPE:\n")
cat("Scenario ID:", representative$median$scenario_id, "\n")
cat("AE Score:", round(representative$median$AE_Percentiles, 2), "\n")
cat("Rank:", representative$median$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")

cat("LOW PERFORMANCE LANDSCAPE:\n")
cat("Scenario ID:", representative$low$scenario_id, "\n")
cat("AE Score:", round(representative$low$AE_Percentiles, 2), "\n")
cat("Rank:", representative$low$Rank_Percentiles, "/", nrow(agg_ae), "\n\n")
