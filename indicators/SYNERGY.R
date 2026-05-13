# Synergy score: discretises each indicator into K classes then sums the class ranks.
# Higher total = more indicators performing well simultaneously.
# Also produces a faceted histogram of the class distributions.
calculate_synergy_scores <- function(
  indicators_results,
  K          = 5,
  rename_map = c(
    RECYCLING    = "final_N_total_per_ha",
    INPUT        = "total_N_legumes",
    SOIL_HEALTH  = "soc_stocks",
    BIODIV       = "biodiv_score",
    ECONOMIC_DIV = "economic_diversification_score",
    FAIRNESS     = "equity_score",
    CONNECT      = "connectivity_score"
  ),
  higher_better = c(
    RECYCLING = TRUE, INPUT = TRUE, SOIL_HEALTH = TRUE, BIODIV = TRUE,
    ECONOMIC_DIV = TRUE, FAIRNESS = TRUE, CONNECT = TRUE
  ),
  palette     = "RdYlGn",
  direction   = 1,
  style       = "equal",
  fixed_breaks = NULL
) {
  library(dplyr); library(tidyr); library(ggplot2); library(classInt)

  stopifnot(is.data.frame(indicators_results), "scenario_id" %in% names(indicators_results))
  missing <- setdiff(unname(rename_map), names(indicators_results))
  if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
  if (!setequal(names(rename_map), names(higher_better))) stop("higher_better must match rename_map names.")
  if (K < 2) stop("K must be >= 2.")
  if (style == "fixed" && (is.null(fixed_breaks) || !is.list(fixed_breaks))) {
    stop("For style = 'fixed', provide a named list in 'fixed_breaks'.")
  }

  indicators_named   <- indicators_results %>% rename(!!!rename_map)
  source_cols        <- names(rename_map)

  # Compute class breaks for each indicator
  indicator_breaks <- list()
  for (col in source_cols) {
    x <- as.numeric(indicators_named[[col]])
    x_clean <- x[!is.na(x)]
    if (length(unique(x_clean)) < 2) { indicator_breaks[[col]] <- range(x_clean); next }
    x_to_cut <- if (!higher_better[col]) -x else x
    x_clean2 <- x_to_cut[!is.na(x_to_cut)]
    n_eff    <- min(K, length(unique(x_clean2)))
    breaks_v <- if (style == "fixed") {
      if (!col %in% names(fixed_breaks)) stop("No breaks provided for: ", col)
      fixed_breaks[[col]]
    } else {
      tryCatch(
        classIntervals(x_clean2, n = n_eff, style = style)$brks,
        error = function(e) classIntervals(x_clean2, n = n_eff, style = "quantile")$brks
      )
    }
    indicator_breaks[[col]] <- unique(breaks_v)
  }

  # Assign classes (1 to K)
  scored <- indicators_named %>%
    select(scenario_id, all_of(source_cols)) %>%
    mutate(across(all_of(source_cols), function(x) {
      col  <- cur_column()
      brks <- indicator_breaks[[col]]
      x_cut <- if (!higher_better[col]) -as.numeric(x) else as.numeric(x)
      as.integer(cut(x_cut, breaks = brks, include.lowest = TRUE, labels = FALSE))
    }, .names = "{.col}_class"))

  class_cols <- paste0(source_cols, "_class")

  # Synergy score = sum of classes; normalise to 0-1
  scored <- scored %>%
    mutate(synergy_score_sum = rowSums(across(all_of(class_cols)), na.rm = TRUE))

  mn <- min(scored$synergy_score_sum, na.rm = TRUE)
  mx <- max(scored$synergy_score_sum, na.rm = TRUE)
  scored <- scored %>%
    mutate(synergy_score_norm = if (mx > mn) (synergy_score_sum - mn) / (mx - mn) else 0)

  out_df <- indicators_results %>%
    left_join(scored %>% select(scenario_id, all_of(class_cols), synergy_score_sum, synergy_score_norm),
              by = "scenario_id")

  # Faceted histogram of normalised indicator distributions coloured by class
  normalise_vec <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (diff(r) == 0) return(rep(0.5, length(x)))
    (x - r[1]) / diff(r)
  }

  norm_ind <- indicators_named %>% mutate(across(all_of(source_cols), normalise_vec))

  data_plot <- norm_ind %>%
    select(scenario_id, all_of(source_cols)) %>%
    left_join(scored %>% select(scenario_id, all_of(class_cols)), by = "scenario_id") %>%
    pivot_longer(all_of(source_cols), names_to = "indicator", values_to = "value_norm") %>%
    pivot_longer(all_of(class_cols), names_to = "indicator_class_name", values_to = "class") %>%
    filter(paste0(indicator, "_class") == indicator_class_name) %>%
    select(scenario_id, indicator, value_norm, class)

  breaks_plot <- tibble(indicator = names(indicator_breaks)) %>%
    mutate(
      min_val   = sapply(indicator, function(i) min(indicators_named[[i]], na.rm = TRUE)),
      range_val = sapply(indicator, function(i) diff(range(indicators_named[[i]], na.rm = TRUE))),
      breaks    = indicator_breaks
    ) %>%
    mutate(range_val = ifelse(range_val == 0, 1, range_val)) %>%
    unnest(breaks) %>%
    mutate(breaks_norm = (breaks - min_val) / range_val) %>%
    group_by(indicator) %>%
    filter(breaks_norm > 1e-6 & breaks_norm < 1 - 1e-6) %>%
    ungroup()

  p_classes <- ggplot(data_plot, aes(x = value_norm, fill = as.factor(class))) +
    geom_histogram(bins = 30, alpha = 0.8, color = "white", linewidth = 0.2) +
    geom_vline(data = breaks_plot, aes(xintercept = breaks_norm),
               linetype = "dashed", color = "black", linewidth = 0.8) +
    facet_wrap(~indicator, scales = "free_y", ncol = 2) +
    coord_cartesian(xlim = c(-0.05, 1.05)) +
    scale_fill_brewer(palette = palette, direction = 1, name = "Class",
                      na.translate = FALSE, drop = FALSE) +
    labs(title    = "Synergy indicator — discrete class scoring",
         subtitle = paste0("Method: '", style, "'"),
         x = "Normalised score (0-1)", y = "Number of landscapes") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 11), legend.position = "bottom")

  list(indicators_results = out_df, p_classes = p_classes)
}
