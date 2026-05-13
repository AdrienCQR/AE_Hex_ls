# === SCORE D'AGROÉCOLOGIE PAR CATÉGORISATION PERCENTILES ===

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)

# --- PARAMETRES ---
K <- 4  # Nombre de classes (1 à 4)

# Mapping des 7 principes AE (noms en MAJUSCULES)
ae_principles_map <- c(
  "RECYCLING"    = "system_performance_score",
  "SOIL_HEALTH"  = "soc_normalized", 
  "BIODIV"       = "biodiv_score",
  "ECONOMIC_DIV" = "economic_diversification_score",
  "FAIRNESS"     = "equity_score",
  "CONNECT"      = "connectivity_score",
  "SYNERGY"      = "synergy_score_norm"
)

# Pour tous les principes AE, higher_better = TRUE
higher_better <- c(
  "RECYCLING" = TRUE, "SOIL_HEALTH" = TRUE, "BIODIV" = TRUE,
  "ECONOMIC_DIV" = TRUE, "FAIRNESS" = TRUE, "CONNECT" = TRUE, 
  "SYNERGY" = TRUE
)

# --- VERIFICATION DES DONNEES ---
required_cols <- unname(ae_principles_map)
missing_cols <- setdiff(required_cols, names(indicators_results))
if (length(missing_cols) > 0) {
  stop("Colonnes manquantes dans indicators_results: ", paste(missing_cols, collapse = ", "))
}

# --- 1) HARMONISATION DES NOMS ---
ae_data <- indicators_results %>% 
  rename(!!!ae_principles_map) %>%
  select(scenario_id, all_of(names(ae_principles_map)))

ae_principle_cols <- names(ae_principles_map)

# --- 2) FORMAT LONG POUR LE PLOT ---
ae_long <- ae_data %>%
  pivot_longer(all_of(ae_principle_cols), names_to = "principle", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# Vérification des ranges
rng <- range(ae_long$value, na.rm = TRUE)
cat(sprintf("Range des valeurs: min=%.3f, max=%.3f\n", rng[1], rng[2]))

# Ordre par médiane décroissante pour le plot
order_by_med <- ae_long %>%
  group_by(principle) %>%
  summarise(med = median(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med)) %>% 
  pull(principle)

ae_long <- ae_long %>%
  mutate(principle = factor(principle, levels = order_by_med))

# --- 3) FONCTION DE CATEGORISATION PAR PERCENTILES ---
percentile_class <- function(x, K = 5, higher_better = TRUE) {
  pr <- percent_rank(as.numeric(x))  # [0,1]
  if (!higher_better) pr <- 1 - pr
  brks <- seq(0, 1, length.out = K + 1)
  cut(pr, breaks = brks, include.lowest = TRUE, right = TRUE, labels = FALSE) %>%
    as.integer()  # Classes 1 à K
}

# --- 4) CATEGORISATION DE CHAQUE PRINCIPE ---
ae_categorized <- ae_data %>%
  mutate(across(
    all_of(ae_principle_cols),
    ~ percentile_class(.x, K = K, higher_better = higher_better[cur_column()]),
    .names = "{.col}_class"
  ))

# Noms des colonnes de classes
class_cols <- paste0(ae_principle_cols, "_class")

# --- 5) CALCUL DES SCORES AE FINAUX ---
ae_categorized <- ae_categorized %>%
  mutate(
    # Score brut (somme des classes 0-4)
    AE_score_sum = rowSums(across(all_of(class_cols)), na.rm = TRUE),
    
    # Score normalisé [0-1] (moyenne des classes / max_classe)
    AE_score_norm = (rowMeans(across(all_of(class_cols)), na.rm = TRUE) - 1) / (K - 1),
    
    # Score sur 100
    AE_score_100 = AE_score_norm * 100
  ) %>%
  mutate(
    AE_score_norm = ifelse(is.nan(AE_score_norm), NA_real_, AE_score_norm),
    AE_score_100 = ifelse(is.nan(AE_score_100), NA_real_, AE_score_100)
  )

# Calcul du rang
ae_categorized <- ae_categorized %>%
  mutate(AE_rank = rank(desc(AE_score_100), ties.method = "min"))

# --- 6) DIAGNOSTIC ---
cat("\n=== DIAGNOSTIC DES CLASSES ===\n")
for(col in class_cols) {
  classes <- table(ae_categorized[[col]], useNA = "ifany")
  cat(sprintf("%s: %s\n", col, paste(names(classes), "->", classes, collapse = ", ")))
}

cat(sprintf("\nScore AE final: min=%.1f, max=%.1f, médiane=%.1f\n",
            min(ae_categorized$AE_score_100, na.rm = TRUE),
            max(ae_categorized$AE_score_100, na.rm = TRUE),
            median(ae_categorized$AE_score_100, na.rm = TRUE)))

# --- 7) TABLEAU DE RESULTATS FINAL ---
ae_final_results <- ae_categorized %>%
  select(
    scenario_id,
    # Scores originaux arrondis (noms en MAJUSCULES)
    RECYCLING,
    SOIL_HEALTH,
    BIODIV,
    ECONOMIC_DIV,
    FAIRNESS,
    CONNECT,
    SYNERGY,
    # Classes (0-4)
    RECYCLING_class,
    SOIL_HEALTH_class,
    BIODIV_class,
    ECONOMIC_DIV_class,
    FAIRNESS_class,
    CONNECT_class,
    SYNERGY_class,
    # Scores AE finaux
    AE_score_sum,
    AE_score_norm,
    AE_score_100,
    AE_rank
  ) %>%
  # Arrondir les scores originaux
  mutate(across(RECYCLING:SYNERGY, ~round(., 3)))

# --- 8) RIDGE PLOT AVEC CLASSES COLORÉES ---
brks <- seq(0, 1, length.out = K + 1)
lbls <- paste0(round(head(brks, -1) * 100), "–", round(tail(brks, -1) * 100), "%")

p_ae_classes <- ggplot(
  ae_long,
  aes(
    x = value, y = fct_rev(principle),
    fill = after_stat(cut(ecdf, breaks = brks, include.lowest = TRUE))
  )
) +
  geom_density_ridges_gradient(
    scale = 1.2, color = "black",
    from = 0, to = 1, calc_ecdf = TRUE,
    quantile_lines = FALSE, quantiles = 0.5,
    alpha = 0.6, linewidth = 0.7
  ) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = 0.05)) +
  scale_y_discrete(expand = expansion(add = c(0.3, 0.3))) +
  coord_cartesian(clip = "off") +
  scale_fill_brewer(
    palette = "RdYlGn", direction = 1, drop = FALSE,
    name = "Percentile", labels = lbls
  ) +
  labs(
    title = "AGROECOLOGY PRINCIPLES - Distribution by Percentile Classes",
    x = "Score", y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

# --- 9) PLOT DE DISTRIBUTION DU SCORE AE FINAL ---
p_ae_final <- ggplot(ae_final_results, aes(x = AE_score_sum)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = median(AE_score_sum, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Final Agroecology Scores",
    subtitle = paste0("Based on percentile classification of 7 AE principles | Median = ", 
                      round(median(ae_final_results$AE_score_sum, na.rm = TRUE), 1)),
    x = "Agroecology Score (0-100)", y = "Count"
  ) +
  theme_minimal(base_size = 13)


# === AFFICHAGE ===
print(p_ae_classes)
print(p_ae_final)






# --- RIDGE PLOT DU SCORE AE FINAL SEUL ---

# Calculer les statistiques pour les classes
ae_score_stats <- ae_final_results %>%
  summarise(
    min_val = min(AE_score_sum, na.rm = TRUE),
    q1 = quantile(AE_score_sum, 0.25, na.rm = TRUE),
    median_val = median(AE_score_sum, na.rm = TRUE),
    q3 = quantile(AE_score_sum, 0.75, na.rm = TRUE),
    max_val = max(AE_score_sum, na.rm = TRUE),
    mean_val = mean(AE_score_sum, na.rm = TRUE)
  )

# Définir les classes pour le score AE final
K_final <- 4  # Nombre de classes
brks_final <- seq(ae_score_stats$min_val, ae_score_stats$max_val, length.out = K_final + 1)
lbls_final <- paste0(round(head(brks_final, -1)), "–", round(tail(brks_final, -1)))

p_ae_final_ridge <- ggplot(
  ae_final_results,
  aes(
    x = AE_score_sum, y = "AE Final Score",
    fill = after_stat(cut(ecdf, breaks = seq(0, 1, length.out = K_final + 1), include.lowest = TRUE))
  )
) +
  geom_density_ridges_gradient(
    scale = 1.5, color = "black",
    from = ae_score_stats$min_val, 
    to = ae_score_stats$max_val, 
    calc_ecdf = TRUE,
    alpha = 0.7, 
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = c(ae_score_stats$min_val - 1, ae_score_stats$max_val + 1), 
    expand = expansion(mult = 0.02)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2))) +
  coord_cartesian(clip = "off") +
  scale_fill_brewer(
    palette = "RdYlGn", direction = 1, drop = FALSE,
    name = "Score Range", labels = lbls_final
  ) +
  labs(
    title = "FINAL AGROECOLOGY SCORE - Distribution with Percentile Classes",
    subtitle = paste0(
      "Range: ", round(ae_score_stats$min_val, 1), "–", round(ae_score_stats$max_val, 1),
      " | Median: ", round(ae_score_stats$median_val, 1),
      " | Mean: ", round(ae_score_stats$mean_val, 1)
    ),
    x = "Agroecology Score (Sum of 7 principles)", 
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# Affichage
print(p_ae_final_ridge)

# --- STATISTIQUES DÉTAILLÉES (optionnel) ---
cat("=== STATISTIQUES DU SCORE AE FINAL ===\n")
print(summary(ae_final_results$AE_score_sum))
cat("\nÉcart-type:", round(sd(ae_final_results$AE_score_sum, na.rm = TRUE), 2), "\n")












str(ae_final_results)

# Renommer la clé dans agg_ae pour matcher
ae_final_results <- ae_final_results %>% rename(id = scenario_id)



str(merged_data)
merged_data$id <- as.character(merged_data$id)




# Left join (garde toutes les lignes de merged_data)
merged_ae <- merged_data %>%
  left_join(ae_final_results, by = "id", suffix = c("_old", ""))
# Supprimer les colonnes _old si nécessaire
old_cols <- grep("_old$", names(merged_ae), value = TRUE)
if(length(old_cols) > 0) {
  merged_ae <- merged_ae %>% select(-all_of(old_cols))
}





# Liste des indicateurs à visualiser
synergy_final <- c("AE_score_sum")

# Créer une liste de graphiques pour chaque indicateur
plots <- lapply(synergy_final, function(synergy_final) {
  ggplot(merged_ae) +
    geom_sf(aes(fill = .data[[synergy_final]])) +
    scale_fill_viridis(option = "turbo", direction = -1, name = synergy_final) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = synergy_final, fill = "")
})

# Donner des noms aux graphiques pour le patchwork
names(plots) <- synergy_final

# Arranger les graphiques dans une grille 2x3
final_plot <- wrap_plots(plots, ncol = 3, nrow = 2)


# Afficher le plot
print(plots)






summary(agg_ae)
