


library(tidyverse)

library(ggplot2)
library(dplyr)
library(reshape2)
library(dplyr)
library(classInt)


calculate_agroecology_composite_coinr <- function(
    indicators_results,
    weights = NULL,  # Poids pour chaque principe (par défaut égaux)
    print_plot = TRUE,
    min_offset = 0.00001,  # Offset minimum pour éviter les zéros
    K = 4  # Nombre de classes pour la méthode percentiles
) {
  
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
  
  # ========== NOUVELLE MÉTHODE: CATÉGORISATION PERCENTILES ==========
  
  # Pour tous les principes AE, higher_better = TRUE
  higher_better <- c(
    "RECYCLING" = TRUE, "SOIL_HEALTH" = TRUE, "BIODIV" = TRUE,
    "ECONOMIC_DIV" = TRUE, "FAIRNESS" = TRUE, "CONNECT" = TRUE, 
    "SYNERGY" = TRUE
  )
  
  # Fonction de catégorisation par percentiles
  percentile_class <- function(x, K = 4, higher_better = TRUE) {
    pr <- percent_rank(as.numeric(x))  # [0,1]
    if (!higher_better) pr <- 1 - pr
    brks <- seq(0, 1, length.out = K + 1)
    cut(pr, breaks = brks, include.lowest = TRUE, right = TRUE, labels = FALSE) %>%
      as.integer()  # Classes 1 à K
  }
  
  # Catégorisation de chaque principe
  data_percentiles <- data_complete %>%
    mutate(across(
      all_of(ae_principles),
      ~ percentile_class(.x, K = K, higher_better = higher_better[cur_column()]),
      .names = "{.col}_class"
    ))
  
  # Noms des colonnes de classes
  class_cols <- paste0(ae_principles, "_class")
  
  # Calcul des scores AE par percentiles
  data_percentiles <- data_percentiles %>%
    mutate(
      # Score brut (somme des classes 0 à K-1)
      AE_score_sum_raw = rowSums(across(all_of(class_cols)), na.rm = TRUE),
      
      # Score normalisé [0-1] (moyenne des classes / max_classe)
      AE_score_norm_raw = (rowMeans(across(all_of(class_cols)), na.rm = TRUE) - 1) / (K - 1),
      
      # Score sur 100
      AE_Percentiles = AE_score_norm_raw * 100
    ) %>%
    mutate(
      AE_score_norm_raw = ifelse(is.nan(AE_score_norm_raw), NA_real_, AE_score_norm_raw),
      AE_Percentiles = ifelse(is.nan(AE_Percentiles), NA_real_, AE_Percentiles)
    )
  
  # ========== MÉTHODES COINr EXISTANTES ==========
  
  # Extraire la matrice des données pour COINr
  X <- data_complete[, ae_principles]
  
  # Vérifier et afficher les valeurs problématiques
  cat("=== DIAGNOSTIC DES DONNÉES ===\n")
  for(col in ae_principles) {
    min_val <- min(X[[col]], na.rm = TRUE)
    max_val <- max(X[[col]], na.rm = TRUE)
    n_zeros <- sum(X[[col]] == 0, na.rm = TRUE)
    n_neg <- sum(X[[col]] < 0, na.rm = TRUE)
    cat(sprintf("%s: min=%.3f, max=%.3f, zeros=%d, negative=%d\n", 
                col, min_val, max_val, n_zeros, n_neg))
  }
  
  # Appliquer l'offset AVANT la normalisation pour éviter les zéros et valeurs négatives
  X_safe <- X %>%
    mutate(across(everything(), ~pmax(. + min_offset, min_offset)))
  
  cat("\n=== APRÈS CORRECTION ===\n")
  for(col in ae_principles) {
    min_val <- min(X_safe[[col]], na.rm = TRUE)
    max_val <- max(X_safe[[col]], na.rm = TRUE)
    cat(sprintf("%s: min=%.3f, max=%.3f\n", col, min_val, max_val))
  }
  
  # Définir les poids (par défaut égaux)
  if (is.null(weights)) {
    weights <- rep(1, length(ae_principles))
  }
  names(weights) <- ae_principles
  
  # Normaliser les données avec COINr (0.1 à 100 pour éviter les zéros après normalisation)
  X_normalised <- COINr::Normalise(X_safe, global_specs = list(f_n = "n_minmax", 
                                                               f_n_para = list(l_u = c(0.1, 100))))
  
  # Vérifier que la normalisation a bien fonctionné
  cat("\n=== APRÈS NORMALISATION ===\n")
  for(col in ae_principles) {
    min_val <- min(X_normalised[[col]], na.rm = TRUE)
    max_val <- max(X_normalised[[col]], na.rm = TRUE)
    cat(sprintf("%s: min=%.3f, max=%.3f\n", col, min_val, max_val))
  }
  
  # Calculer les scores agrégés avec différentes méthodes
  
  # 1. Moyenne arithmétique
  score_amean <- COINr::Aggregate(X_normalised, f_ag = "a_amean", 
                                  f_ag_para = list(w = weights))
  
  # 2. Moyenne géométrique (maintenant sûre car toutes les valeurs > 0)
  score_gmean <- COINr::Aggregate(X_normalised, f_ag = "a_gmean", 
                                  f_ag_para = list(w = weights))
  
  # 3. Moyenne harmonique
  score_hmean <- COINr::Aggregate(X_normalised, f_ag = "a_hmean", 
                                  f_ag_para = list(w = weights))
  
  # Normaliser les scores finaux pour qu'ils soient entre 0 et 100
  score_amean_norm <- COINr::Normalise(data.frame(score = score_amean), 
                                       global_specs = list(f_n = "n_minmax", 
                                                           f_n_para = list(l_u = c(0, 100))))$score
  
  score_gmean_norm <- COINr::Normalise(data.frame(score = score_gmean), 
                                       global_specs = list(f_n = "n_minmax", 
                                                           f_n_para = list(l_u = c(0, 100))))$score
  
  score_hmean_norm <- COINr::Normalise(data.frame(score = score_hmean), 
                                       global_specs = list(f_n = "n_minmax", 
                                                           f_n_para = list(l_u = c(0, 100))))$score
  
  # ========== CONSOLIDATION DES RÉSULTATS ==========
  
  # Combiner toutes les méthodes dans un dataframe final
  ae_results <- data.frame(
    scenario_id = data_complete$scenario_id,
    
    # Scores individuels normalisés (de X_normalised, ramenés à 0-100)
    COINr::Normalise(X_normalised, global_specs = list(f_n = "n_minmax", 
                                                       f_n_para = list(l_u = c(0, 100)))),
    
    # Classes percentiles (de data_percentiles)
    data_percentiles[, class_cols],
    
    # --- SCORES COMPOSITES ---
    # Méthodes COINr
    AE_Arithmetic = score_amean_norm,
    AE_Geometric = score_gmean_norm,
    AE_Harmonic = score_hmean_norm,
    
    # Méthode percentiles
    AE_Percentiles = data_percentiles$AE_Percentiles,
    AE_score_sum_raw = data_percentiles$AE_score_sum_raw,
    
    
    stringsAsFactors = FALSE
  )
  
  # ========== CALCUL DES RANGS ==========
  score_columns <- c("AE_Arithmetic", "AE_Geometric", "AE_Harmonic", "AE_Percentiles")
  rank_columns <- paste0("Rank_", gsub("AE_", "", score_columns))
  
  for(i in seq_along(score_columns)) {
    ae_results[[rank_columns[i]]] <- rank(desc(ae_results[[score_columns[i]]]), ties.method = "min")
  }
  
  # ========== ANALYSES DE CORRÉLATION ==========
  cor_matrix_scores <- cor(ae_results[, score_columns], use = "complete.obs")
  cor_matrix_ranks <- cor(ae_results[, rank_columns], use = "complete.obs", method = "spearman")
  
  # ========== DIAGNOSTICS ==========
  cat("\n=== DIAGNOSTIC FINAL - MÉTHODE PERCENTILES ===\n")
  for(col in class_cols) {
    classes <- table(data_percentiles[[col]], useNA = "ifany")
    cat(sprintf("%s: %s\n", col, paste(names(classes), "->", classes, collapse = ", ")))
  }
  
  cat("\n=== STATISTIQUES DES SCORES FINAUX ===\n")
  for(col in score_columns) {
    score_stats <- summary(ae_results[[col]])
    cat(sprintf("%s: min=%.1f, median=%.1f, max=%.1f\n", 
                col, score_stats[1], score_stats[3], score_stats[6]))
  }
  
  cat("\n=== CORRÉLATIONS ENTRE MÉTHODES ===\n")
  print(round(cor_matrix_scores, 3))
  
  # ========== VISUALISATIONS ==========
  
  # 1. Ridge plot avec les 4 méthodes
  ae_scores_long <- ae_results %>%
    select(scenario_id, all_of(score_columns)) %>%
    pivot_longer(-scenario_id, names_to = "Method", values_to = "Score") %>%
    mutate(Method = gsub("AE_", "", Method))
  
  # Ordre par médiane décroissante
  method_order <- ae_scores_long %>%
    group_by(Method) %>%
    summarise(median_score = median(Score, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(median_score)) %>%
    pull(Method)
  
  ae_scores_long <- ae_scores_long %>%
    mutate(Method = factor(Method, levels = method_order))
  
  p_ridges <- ggplot(ae_scores_long, aes(x = Score, y = Method, fill = Method)) +
    geom_density_ridges(alpha = 0.7, color = "black", scale = 2, linewidth = 0.7
) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = 0.02)) +
    scale_fill_viridis_d(option = "plasma", name = "Method") +
    coord_cartesian(clip = "off") +
    labs(
      title = "Distribution of Agroecology Scores by Aggregation Method",
      subtitle = paste0("n = ", nrow(ae_results), " landscapes"),
      x = "Agroecology Score (0-100)", 
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(face = "bold"),
      legend.position = "none"
    )
  
  # 2. GGally plot pour les scores (mis à jour avec 4 méthodes)
  ggally_scores <- ae_results %>%
    select(all_of(score_columns)) %>%
    rename_with(~gsub("AE_", "", .), everything())
  
  p_ggally_scores <- GGally::ggpairs(
    ggally_scores,
    title = "Pairwise Comparison of Agroecology Scores (0-100) - 4 Methods",
    upper = list(continuous = GGally::wrap("cor", size = 4, color = "blue")),  # ou ggally_cor
    lower = list(continuous = GGally::wrap("smooth", alpha = 0.3, color = "red")),  # ou ggally_smooth
    diag = list(continuous = GGally::wrap("densityDiag", fill = "lightblue", alpha = 0.7))  # ou ggally_densityDiag
  ) + theme_minimal(base_size = 11)
  
  # 3. GGally plot pour les rangs (mis à jour avec 4 méthodes)
  ggally_ranks <- ae_results %>%
    select(all_of(rank_columns)) %>%
    rename_with(~gsub("Rank_", "", .), everything())
  
  p_ggally_ranks <- GGally::ggpairs(
    ggally_ranks,
    title = "Pairwise Comparison of Agroecological Rankings - 4 Methods",
    upper = list(continuous = GGally::wrap("cor", size = 4, color = "darkgreen", method = "spearman")),
    lower = list(continuous = GGally::wrap("smooth", alpha = 0.3, color = "orange")),
    # Add this line to specify the diagonal plot type
    diag = list(continuous = GGally::wrap("barDiag", bins = 20, color = "black", fill="lightblue")) 
  ) +
    theme_minimal(base_size = 11)
  
  # 4. Heatmap des corrélations (mise à jour)
  cor_heatmap_data <- cor_matrix_scores %>%
    as.data.frame() %>%
    rownames_to_column("Method1") %>%
    pivot_longer(-Method1, names_to = "Method2", values_to = "Correlation") %>%
    mutate(
      Method1 = gsub("AE_", "", Method1),
      Method2 = gsub("AE_", "", Method2)
    )
  
  p_heatmap <- ggplot(cor_heatmap_data, aes(x = Method1, y = Method2, fill = Correlation)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = round(Correlation, 3)), color = "white", size = 5, fontface = "bold") +
    scale_fill_viridis_c(option = "rocket", 
                         direction = -1,  # Inverse la palette
                         name = "Pearson\nCorrelation",
                         limits = c(0, 1),
                         guide = guide_colorbar(barwidth = 1, barheight = 6)) +
    labs(
      title = "Correlation Matrix: Agroecology Scores - 4 Methods",
      subtitle = "Correlations between different aggregation methods",
      x = "", y = ""
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid = element_blank()
    )
  
  # 5. Tableau de synthèse des top 10 (par méthode arithmétique)
  top_scenarios <- ae_results %>%
    arrange(Rank_Arithmetic) %>%
    slice_head(n = 10)
  
  # Affichage des graphiques
  if (isTRUE(print_plot)) {
    print(p_ridges)
    print(p_ggally_scores)
    print(p_ggally_ranks)  
    print(p_heatmap)
    cat("\n=== TOP 10 SCENARIOS (by Arithmetic Mean) ===\n")
    print(top_scenarios)
  }
  
  # Retour des résultats
  return(list(
    # Données principales
    ae_results = ae_results,
    top_scenarios = top_scenarios,
    
    # Matrices de corrélation
    cor_scores = cor_matrix_scores,
    cor_ranks = cor_matrix_ranks,
    
    # Graphiques
    p_ridges = p_ridges,
    p_ggally_scores = p_ggally_scores,
    p_ggally_ranks = p_ggally_ranks,
    p_heatmap = p_heatmap,
    
    # Informations
    weights_used = weights,
    n_scenarios = nrow(ae_results),
    offset_used = min_offset,
    K_classes = K
  ))
}

# ========== UTILISATION ==========
# Avec la nouvelle méthode percentiles intégrée
results <- calculate_agroecology_composite_coinr(indicators_results, 
                                                 min_offset = 0.0001, 
                                                 K = 4)

# Le dataframe final avec toutes les méthodes
agg_ae <- results$ae_results





summary(agg_ae$AE_score_sum_raw)
summary(agg_ae)

str(agg_ae)





summary(agg_ae)



library(ggplot2)
library(dplyr)
library(reshape2)

# Sélectionner uniquement les indicateurs AE individuels
ae_indicators <- c("RECYCLING", "SOIL_HEALTH", "BIODIV", "ECONOMIC_DIV", "FAIRNESS", "CONNECT", "SYNERGY")

# Créer la matrice de corrélation
correlation_matrix <- cor(agg_ae[ae_indicators], use = "complete.obs")

# Convertir en format long pour ggplot
correlation_long <- melt(correlation_matrix)
colnames(correlation_long) <- c("Indicator1", "Indicator2", "correlation")

# Créer la heatmap de corrélation
correlation_heatmap <- ggplot(
  data = correlation_long, 
  aes(x = Indicator1, y = Indicator2, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 4) +
  
  # Utilisation d'une palette RColorBrewer
  scale_fill_distiller(
    palette = "RdBu",     # Palette Rouge-Bleu
    limit = c(-1, 1),     # Échelle complète de -1 à 1
    name = "Correlation"
  ) +
  
  # Thème et ajustements esthétiques
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 20)),
    axis.title = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  
  # Titres
  labs(
    title = "Correlation Matrix between AE Indicators",
    subtitle = "Pearson correlation matrix"
  ) +
  coord_fixed(ratio = 1)

# Afficher la heatmap
print(correlation_heatmap)

# Optionnel : Version triangulaire supérieure uniquement
correlation_matrix_upper <- correlation_matrix
correlation_matrix_upper[lower.tri(correlation_matrix_upper)] <- NA

correlation_long_upper <- melt(correlation_matrix_upper)
colnames(correlation_long_upper) <- c("Indicator1", "Indicator2", "correlation")
correlation_long_upper <- correlation_long_upper[!is.na(correlation_long_upper$correlation), ]

# Heatmap triangulaire
correlation_heatmap_triangular <- ggplot(
  data = correlation_long_upper, 
  aes(x = Indicator1, y = Indicator2, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 4) +
  
  scale_fill_distiller(
    palette = "RdBu",
    limit = c(-1, 1),
    name = "Correlation"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 20)),
    axis.title = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  
  labs(
    title = "Correlation Matrix between AE Indicators",
    subtitle = "Pearson correlation matrix"
  ) +
  coord_fixed(ratio = 1)

print(correlation_heatmap_triangular)






