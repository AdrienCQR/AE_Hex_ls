# =============================================================================
# 04_synergies.R
# =============================================================================
# Computes pairwise synergies, trade-offs, and losses between AE indicators
# across all hexagonal landscape units, following the approach of:
#   Leroux et al. (2022) — categorical classification of indicator levels.
#
# Method:
#   Each indicator is classified into 4 quantile classes (1 = low, 4 = high).
#   For each pair of indicators, relationships are defined as:
#     - Synergy   : both high (class 4)
#     - Loss      : both low  (class 1)
#     - Trade-off : one high, one low
#
# Requires: 00_config.R already sourced (done by RUN_MAIN.R)
#
# Inputs:
#   AE_Hex_ls/results/v3_final_results_with_composite_scores.csv
#   AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp
#
# Outputs: PNG figures in AE_Hex_ls/results/maps3/
# =============================================================================

# --- Packages ----------------------------------------------------------------
pkgs <- c(
  "sf", "ggplot2", "dplyr", "tidyr", "classInt",
  "patchwork", "reshape2", "viridis", "forcats", "gridExtra"
)
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}
invisible(lapply(pkgs, library, character.only = TRUE))

output_dir <- FILE_PATHS$maps_dir
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Load data ----------------------------------------------------------------

hex_grid <- st_read(FILE_PATHS$hex_grid_v4, quiet = TRUE)
hex_grid$id <- as.character(hex_grid$id)

indicators_results <- read.csv(FILE_PATHS$output_results)
indicators_results$scenario_id <- as.character(indicators_results$scenario_id)

map_data <- left_join(hex_grid, indicators_results, by = c("id" = "scenario_id"))

# =============================================================================
# 1. Classify each indicator into quantile classes (1 to 4)
# =============================================================================

norm_indicators <- c(
  "RECYCLING_norm", "INPUT_norm", "SOIL_HEALTH_norm", "BIODIV_norm",
  "ECONOMIC_DIV_norm", "FAIRNESS_norm", "CONNECT_norm", "SYNERGY_norm"
)

classify_quantile <- function(x, n = 4) {
  x_clean <- x[!is.na(x)]
  if (length(unique(x_clean)) < n) n <- length(unique(x_clean))
  breaks <- unique(classIntervals(x_clean, n = n, style = "quantile")$brks)
  as.integer(cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE))
}

for (ind in norm_indicators) {
  class_col <- sub("_norm$", "_class", ind)
  map_data[[class_col]] <- classify_quantile(map_data[[ind]])
}

cat("  Indicator classes computed.\n")

# =============================================================================
# 2. Reclassify into high / moderate / low and compute pairwise relationships
# =============================================================================

es_indicators <- c(
  "RECYCLING_class", "INPUT_class", "SOIL_HEALTH_class", "BIODIV_class",
  "ECONOMIC_DIV_class", "FAIRNESS_class", "CONNECT_class", "SYNERGY_class"
)

reclassify_scores <- function(score) {
  dplyr::case_when(
    score == 4         ~ "high",
    score %in% c(2, 3) ~ "moderate",
    score == 1         ~ "low"
  )
}

data_analysis <- map_data %>%
  select(id, all_of(es_indicators))

data_reclassified <- data_analysis %>%
  mutate(across(all_of(es_indicators), reclassify_scores, .names = "{.col}_reclass"))

reclass_cols  <- paste0(es_indicators, "_reclass")
combinations  <- combn(reclass_cols, 2, simplify = FALSE)

determine_relationship <- function(s1, s2) {
  if (is.na(s1) | is.na(s2)) return("no_relationship")
  if (s1 == "high" & s2 == "high") return("synergy")
  if (s1 == "low"  & s2 == "low")  return("loss")
  if ((s1 == "high" & s2 == "low") | (s1 == "low" & s2 == "high")) return("trade_off")
  "no_relationship"
}

data_results <- data_reclassified %>%
  mutate(synergy_count = 0L, trade_off_count = 0L, loss_count = 0L, no_relationship_count = 0L)

for (i in seq_len(nrow(data_results))) {
  syn <- to <- lo <- nr <- 0L
  for (combo in combinations) {
    rel <- determine_relationship(
      data_results[[combo[1]]][i],
      data_results[[combo[2]]][i]
    )
    if (rel == "synergy")         syn <- syn + 1L
    else if (rel == "trade_off")  to  <- to  + 1L
    else if (rel == "loss")       lo  <- lo  + 1L
    else                          nr  <- nr  + 1L
  }
  data_results[i, "synergy_count"]         <- syn
  data_results[i, "trade_off_count"]       <- to
  data_results[i, "loss_count"]            <- lo
  data_results[i, "no_relationship_count"] <- nr
}

cat("  Total indicator pairs analysed:", length(combinations), "\n")

# =============================================================================
# 3. Classify intensity (none / weak / moderate / strong) using Jenks
# =============================================================================

classify_intensity <- function(values) {
  zero_idx <- which(values == 0)
  nonzero  <- values[values > 0]
  result   <- character(length(values))
  result[zero_idx] <- "none"

  if (length(nonzero) == 0) {
    return(factor(rep("none", length(values)), levels = c("none", "weak", "moderate", "strong")))
  }

  if (length(unique(nonzero)) == 1) {
    result[values > 0] <- "weak"
  } else if (length(unique(nonzero)) == 2) {
    med_val <- median(nonzero)
    result[values > 0] <- ifelse(values[values > 0] <= med_val, "weak", "strong")
  } else {
    breaks <- classIntervals(nonzero, n = 3, style = "jenks")$brks
    breaks[1] <- breaks[1] - 0.001
    classified <- cut(
      values[values > 0],
      breaks = breaks,
      labels = c("weak", "moderate", "strong"),
      include.lowest = TRUE, right = TRUE
    )
    result[values > 0] <- as.character(classified)
  }

  factor(result, levels = c("none", "weak", "moderate", "strong"))
}

data_results <- data_results %>%
  mutate(
    synergy_intensity   = classify_intensity(synergy_count),
    trade_off_intensity = classify_intensity(trade_off_count),
    loss_intensity      = classify_intensity(loss_count)
  )

cat("  Intensity distributions:\n")
cat("    Synergy  :"); print(table(data_results$synergy_intensity))
cat("    Trade-off:"); print(table(data_results$trade_off_intensity))
cat("    Loss     :"); print(table(data_results$loss_intensity))

# =============================================================================
# 4. Determine dominant relationship per hexagon (based on raw counts)
# =============================================================================

data_results <- data_results %>%
  mutate(
    dominant_relationship = case_when(
      synergy_count > trade_off_count & synergy_count > loss_count     ~ "Synergies",
      trade_off_count > synergy_count & trade_off_count > loss_count   ~ "Trade-offs",
      loss_count > synergy_count & loss_count > trade_off_count        ~ "Losses",
      synergy_count == trade_off_count & synergy_count > loss_count    ~ "Mixed (Syn-TO)",
      trade_off_count == loss_count & trade_off_count > synergy_count  ~ "Mixed (TO-Loss)",
      TRUE                                                              ~ "No relation"
    ),
    dominant_relationship = factor(dominant_relationship)
  )

cat("  Dominant relationship distribution:\n")
print(table(data_results$dominant_relationship))

# =============================================================================
# 5. Join results back to spatial hex grid
# =============================================================================

join_cols <- c(
  "id", "synergy_count", "trade_off_count", "loss_count", "no_relationship_count",
  "synergy_intensity", "trade_off_intensity", "loss_intensity", "dominant_relationship"
)

map_data <- map_data %>%
  left_join(
    data_results %>%
      st_drop_geometry() %>%
      select(all_of(join_cols)),
    by = "id"
  ) %>%
  mutate(
    synergy_intensity   = factor(synergy_intensity,   levels = c("none", "weak", "moderate", "strong")),
    trade_off_intensity = factor(trade_off_intensity, levels = c("none", "weak", "moderate", "strong")),
    loss_intensity      = factor(loss_intensity,      levels = c("none", "weak", "moderate", "strong")),
    dominant_relationship = factor(dominant_relationship)
  )

# =============================================================================
# 6. Visualisation helpers
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

create_map <- function(
    data,
    variable,
    title = NULL,
    subtitle = NULL,
    legend_title = NULL,
    n_classes = 4,
    style = "quantile",
    palette = "viridis",
    custom_colors = NULL,
    category_order = NULL,
    show_distribution_legend = FALSE,
    dist_legend_title = "Distribution",
    dist_legend_position = c(0.7, 0.7, 0.90, 0.90)
) {
  if (!variable %in% names(data)) stop(paste("Variable '", variable, "' not found."))
  var_sym <- rlang::sym(variable)
  is_categorical <- is.factor(data[[variable]]) || is.character(data[[variable]])

  if (is_categorical) {
    if (!is.null(category_order))
      data <- data %>% mutate(!!var_sym := factor(!!var_sym, levels = category_order))

    map_plot <- ggplot(data = data) +
      geom_sf(aes(fill = !!var_sym), color = "white", linewidth = 0) +
      theme_void(base_size = 12) +
      labs(title = title, subtitle = subtitle, fill = legend_title %||% variable)

    if (!is.null(custom_colors))
      map_plot <- map_plot + scale_fill_manual(values = custom_colors, drop = FALSE)

    if (show_distribution_legend) {
      map_plot <- map_plot + theme(legend.position = "none")

      dist_data <- data %>%
        sf::st_drop_geometry() %>%
        dplyr::count(!!var_sym, .drop = FALSE) %>%
        mutate(percentage = n / sum(n) * 100)

      if (!is.null(category_order))
        dist_data <- dist_data %>%
          mutate(!!var_sym := factor(!!var_sym, levels = rev(category_order)))

      dist_plot <- ggplot(dist_data, aes(y = !!var_sym, x = percentage, fill = !!var_sym)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentage, 1), "%")),
                  hjust = -0.1, size = 3.5, color = "black", fontface = "bold") +
        labs(x = NULL, y = NULL, title = dist_legend_title) +
        scale_x_continuous(expand = c(0, 0.05)) +
        theme_minimal(base_size = 9) +
        theme(
          plot.title   = element_text(hjust = 0, size = 12, face = "bold"),
          panel.grid   = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks   = element_blank(),
          axis.text.y  = element_text(size = 12),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA)
        ) +
        coord_cartesian(clip = "off")

      if (!is.null(custom_colors))
        dist_plot <- dist_plot + scale_fill_manual(values = custom_colors, drop = FALSE)

      final_plot <- map_plot +
        patchwork::inset_element(
          dist_plot,
          left   = dist_legend_position[1],
          bottom = dist_legend_position[2],
          right  = dist_legend_position[3],
          top    = dist_legend_position[4],
          align_to = "plot"
        )
      final_plot <- final_plot &
        theme(plot.background = element_rect(fill = "transparent", color = NA))
      return(final_plot)

    } else {
      map_plot <- map_plot +
        theme(plot.background = element_rect(fill = "transparent", color = NA))
      return(map_plot)
    }

  } else {
    data_clean <- data %>% filter(!is.na(!!var_sym))
    breaks <- classInt::classIntervals(
      data_clean[[variable]], n = n_classes, style = style
    )$brks

    map_plot <- ggplot(data = data) +
      geom_sf(aes(fill = !!var_sym), color = "white", linewidth = 0.1) +
      scale_fill_viridis_b(
        name     = legend_title %||% variable,
        option   = palette,
        breaks   = breaks,
        labels   = scales::number_format(accuracy = 0.1),
        direction = -1
      ) +
      theme_void(base_size = 12) +
      labs(title = title, subtitle = subtitle) +
      theme(plot.background = element_rect(fill = "transparent", color = NA))
    return(map_plot)
  }
}

# =============================================================================
# 7. Map — dominant relationship
# =============================================================================

dominant_colors <- c(
  "Synergies"      = "#00496FFF",
  "Trade-offs"     = "#EDD746FF",
  "Losses"         = "#DD4124FF",
  "No relation"    = "grey85",
  "Mixed (Syn-TO)" = "#0F85A0FF",
  "Mixed (TO-Loss)"= "#ED8B00FF"
)
relationship_order <- c(
  "No relation", "Losses", "Mixed (TO-Loss)", "Trade-offs", "Mixed (Syn-TO)", "Synergies"
)

map_dominant <- create_map(
  data             = map_data,
  variable         = "dominant_relationship",
  title            = "Dominant Relationship",
  custom_colors    = dominant_colors,
  category_order   = relationship_order,
  show_distribution_legend = FALSE
)

ggsave(
  file.path(output_dir, "map_map_dominant_relation.png"),
  map_dominant, width = 8, height = 8, dpi = 300, bg = "transparent"
)
cat("  Saved: map_map_dominant_relation.png\n")

# =============================================================================
# 8. Maps — intensity per relationship type (individual files)
# =============================================================================

intensity_colors_synergy <- c(
  "none"     = "grey85",
  "weak"     = "#d1e5f0",
  "moderate" = "#67a9cf",
  "strong"   = "#2166ac"
)
intensity_colors_tradeoff <- c(
  "none"     = "grey85",
  "weak"     = "#FFF9C4FF",
  "moderate" = "#FFF176FF",
  "strong"   = "#FBC02DFF"
)
intensity_colors_loss <- c(
  "none"     = "grey85",
  "weak"     = "#fddbc7",
  "moderate" = "#ef8a62",
  "strong"   = "#b2182b"
)

map_synergy_intensity <- create_map(
  data          = map_data,
  variable      = "synergy_intensity",
  title         = "",
  legend_title  = "Synergies\nIntensity",
  custom_colors = intensity_colors_synergy
)
map_tradeoff_intensity <- create_map(
  data          = map_data,
  variable      = "trade_off_intensity",
  title         = "",
  legend_title  = "Trade-offs\nIntensity",
  custom_colors = intensity_colors_tradeoff
)
map_loss_intensity <- create_map(
  data          = map_data,
  variable      = "loss_intensity",
  title         = "",
  legend_title  = "Losses\nIntensity",
  custom_colors = intensity_colors_loss
)

ggsave(file.path(output_dir, "map_map_synergy_intensity.png"),
       map_synergy_intensity,  width = 4, height = 4, dpi = 300, bg = "transparent")
ggsave(file.path(output_dir, "map_map_tradeoff_intensity.png"),
       map_tradeoff_intensity, width = 4, height = 4, dpi = 300, bg = "transparent")
ggsave(file.path(output_dir, "map_map_loss_intensity.png"),
       map_loss_intensity,     width = 4, height = 4, dpi = 300, bg = "transparent")
cat("  Saved: intensity maps (synergy, tradeoff, loss).\n")

# =============================================================================
# 9. Heatmaps — pairwise relationship percentages
# =============================================================================

ae_indicators_base <- c(
  "RECYCLING", "INPUT", "SOIL_HEALTH", "BIODIV",
  "ECONOMIC_DIV", "FAIRNESS", "CONNECT", "SYNERGY"
)

hex_flat <- map_data %>% st_drop_geometry()
n_ind    <- length(ae_indicators_base)
syn_mat  <- matrix(NA, n_ind, n_ind, dimnames = list(ae_indicators_base, ae_indicators_base))
to_mat   <- matrix(NA, n_ind, n_ind, dimnames = list(ae_indicators_base, ae_indicators_base))
loss_mat <- matrix(NA, n_ind, n_ind, dimnames = list(ae_indicators_base, ae_indicators_base))

for (i in seq_len(n_ind - 1)) {
  for (j in (i + 1):n_ind) {
    col1 <- paste0(ae_indicators_base[i], "_class")
    col2 <- paste0(ae_indicators_base[j], "_class")
    v1   <- hex_flat[[col1]]
    v2   <- hex_flat[[col2]]
    ok   <- !is.na(v1) & !is.na(v2)
    v1   <- v1[ok]; v2 <- v2[ok]
    n    <- length(v1)
    if (n == 0) next

    diff_vals   <- v1 - v2
    syn_mat[i, j]  <- sum(diff_vals == 0 & v1 > 2)    / n * 100
    to_mat[i, j]   <- sum(abs(diff_vals) >= 2)         / n * 100
    loss_mat[i, j] <- sum(diff_vals == 0 & v1 <= 1)   / n * 100
  }
}

intensity_colors_syn5 <- c("white", "#d1e5f0", "#67a9cf", "#2166ac", "#08306b")
intensity_colors_to5  <- c("white", "#FFF9C4FF", "#FFF176FF", "#FBC02DFF", "#E65100FF")
intensity_colors_lo5  <- c("white", "#fddbc7", "#ef8a62", "#b2182b", "#67001f")

make_heatmap <- function(mat, title, custom_colors) {
  df <- reshape2::melt(mat)
  names(df) <- c("Indicator1", "Indicator2", "Percentage")
  df <- df[!is.na(df$Percentage), ]

  ggplot(df, aes(x = Indicator1, y = Indicator2, fill = Percentage)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = round(Percentage, 1)),
      color = ifelse(df$Percentage > max(df$Percentage) / 1.2, "white", "black"),
      size = 3
    ) +
    scale_fill_gradientn(
      name   = "% of pairs",
      colors = custom_colors,
      values = scales::rescale(c(0, 20, 40, 70, 100)),
      na.value = "grey85"
    ) +
    labs(title = title, x = "", y = "") +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y  = element_text(size = 10),
      plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid   = element_blank()
    ) +
    coord_fixed(ratio = 1)
}

hm_syn  <- make_heatmap(syn_mat,  "a) Synergies between AE Indicators",  intensity_colors_syn5)
hm_to   <- make_heatmap(to_mat,   "b) Trade-offs between AE Indicators", intensity_colors_to5)
hm_loss <- make_heatmap(loss_mat, "c) Losses between AE Indicators",     intensity_colors_lo5)

ggsave(file.path(output_dir, "map_heatmap_synergies.png"),  hm_syn,  width = 5, height = 5, dpi = 300, bg = "transparent")
ggsave(file.path(output_dir, "map_heatmap_tradeoffs.png"),  hm_to,   width = 5, height = 5, dpi = 300, bg = "transparent")
ggsave(file.path(output_dir, "map_heatmap_losses.png"),     hm_loss, width = 5, height = 5, dpi = 300, bg = "transparent")
cat("  Saved: heatmaps (synergies, tradeoffs, losses).\n")

combined_heatmaps <- gridExtra::grid.arrange(hm_syn, hm_to, hm_loss, ncol = 2, nrow = 2)
ggsave(file.path(output_dir, "heatmap_combined.png"),
       combined_heatmaps, width = 12, height = 12, dpi = 300, bg = "white")
cat("  Saved: heatmap_combined.png\n")

# =============================================================================
# 10. Summary statistics
# =============================================================================

dominant_summary <- data_results %>%
  st_drop_geometry() %>%
  group_by(dominant_relationship) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 0))

cat("  Dominant relationship summary:\n")
print(dominant_summary)

total_syn   <- sum(syn_mat,  na.rm = TRUE)
total_to    <- sum(to_mat,   na.rm = TRUE)
total_loss  <- sum(loss_mat, na.rm = TRUE)
total_all   <- total_syn + total_to + total_loss

cat(sprintf(
  "  Global proportions — Synergies: %d%% | Trade-offs: %d%% | Losses: %d%%\n",
  round(total_syn  / total_all * 100),
  round(total_to   / total_all * 100),
  round(total_loss / total_all * 100)
))

print(syn_mat)
print(to_mat)
print(loss_mat)
