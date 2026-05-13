# =============================================================================
# 03_maps.R
# =============================================================================
# Produces choropleth maps for each agroecological indicator.
# Maps use OpenStreetMap as a basemap, Jenks classification, and ward labels.
#
# Requires: 00_config.R already sourced (done by RUN_MAIN.R)
#
# Inputs:
#   AE_Hex_ls/results/v3_final_results_with_composite_scores.csv
#   AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp
#   data/raw/communal_wards/communal_wards.shp
#
# Outputs: PNG files in AE_Hex_ls/results/maps/
# =============================================================================

# --- Packages ----------------------------------------------------------------
pkgs <- c("sf", "terra", "ggplot2", "dplyr", "classInt", "ggspatial", "paletteer", "ggrepel")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Load data ----------------------------------------------------------------

hex_grid <- st_read(FILE_PATHS$hex_grid_v4, quiet = TRUE)
hex_grid$id <- as.character(hex_grid$id)

indicators_results <- read.csv(FILE_PATHS$output_results)

ward_boundaries <- st_read(FILE_PATHS$wards, quiet = TRUE)

output_dir <- FILE_PATHS$maps_dir
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Merge spatial data with results
map_data <- left_join(hex_grid, indicators_results, by = c("id" = "scenario_id"))

# Also save a GeoPackage for GIS use
writeVector(
  vect(map_data),
  "results/map_data_v3.gpkg",
  overwrite = TRUE
)

# --- Mapping function ---------------------------------------------------------

create_discrete_map <- function(
  data,
  variable,
  title          = "",
  legend_title   = variable,
  n_classes      = 5,
  style          = "jenks",
  breaks         = NULL,
  use_paletteer  = FALSE,
  palette        = "cividis",
  paletteer_palette = NULL,
  direction      = 1,
  boundaries_sf  = NULL,
  boundary_color = "grey20",
  boundary_width = 0.6,
  boundary_label = "Ward boundaries",
  add_ward_labels    = TRUE,
  ward_label_column  = "wardname",
  ward_label_size    = 3.5,
  add_scale          = TRUE,
  add_north          = TRUE,
  basemap            = "osm",
  alpha              = 0.8,
  show_bbox          = FALSE,
  legend_frame       = TRUE,
  legend_background_alpha = 0.80,
  legend_key_size    = 0.6,
  legend_text_size   = 11,
  legend_title_size  = 12
) {
  var_data <- data[[variable]]

  # Compute class breaks
  if (style == "fixed") {
    if (is.null(breaks)) stop("Provide 'breaks' when style = 'fixed'.")
  } else {
    var_clean <- var_data[!is.na(var_data)]
    n_classes <- min(n_classes, length(unique(var_clean)))
    breaks <- tryCatch(
      classIntervals(var_clean, n = n_classes, style = style)$brks,
      error = function(e) classIntervals(var_clean, n = n_classes, style = "quantile")$brks
    )
  }

  breaks <- unique(breaks)
  data$binned_variable <- cut(var_data, breaks = breaks, include.lowest = TRUE, right = TRUE)

  p <- ggplot()

  if (!is.null(basemap) && basemap == "osm") {
    p <- p + annotation_map_tile(type = "osm", zoomin = 0, progress = "none", quiet = TRUE)
  }

  p <- p + geom_sf(data = data, aes(fill = binned_variable), color = NA,
                   alpha = ifelse(!is.null(basemap), alpha, 1))

  if (use_paletteer) {
    p <- p + paletteer::scale_fill_paletteer_d(
      palette = paletteer_palette, name = legend_title,
      na.value = "grey80", drop = FALSE, direction = direction,
      guide = guide_legend(reverse = TRUE, override.aes = list(color = NA), order = 1)
    )
  } else {
    p <- p +
      scale_fill_viridis_d(option = palette, name = legend_title,
                           na.value = "grey80", drop = FALSE, direction = direction) +
      guides(fill = guide_legend(reverse = TRUE, override.aes = list(color = NA), order = 1))
  }

  if (!is.null(boundaries_sf)) {
    p <- p +
      geom_sf(data = boundaries_sf, aes(color = boundary_label),
              fill = NA, linewidth = boundary_width, show.legend = TRUE) +
      scale_color_manual(
        name = "",
        values = setNames(boundary_color, boundary_label),
        guide = guide_legend(override.aes = list(fill = NA, linewidth = boundary_width * 1.5), order = 2)
      )

    if (add_ward_labels && ward_label_column %in% colnames(boundaries_sf)) {
      centroids <- st_point_on_surface(boundaries_sf)
      p <- p + ggrepel::geom_text_repel(
        data = centroids,
        aes(label = .data[[ward_label_column]], geometry = geometry),
        stat = "sf_coordinates",
        size = ward_label_size, color = "grey20", fontface = "bold",
        bg.color = "white", bg.r = 0.15,
        force = 0, max.overlaps = 30,
        segment.color = NA, box.padding = 0.3, point.padding = 0.3
      )
    }
  }

  if (add_scale) {
    p <- p + annotation_scale(location = "bl", width_hint = 0.25, style = "ticks",
                              line_col = "black", text_col = "black",
                              pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"))
  }

  if (add_north) {
    p <- p + annotation_north_arrow(
      location = "tl", which_north = "true",
      pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
      style = north_arrow_fancy_orienteering(fill = c("grey40", "white"), line_col = "grey20"),
      height = unit(1.5, "cm"), width = unit(1.5, "cm")
    )
  }

  p <- p +
    theme_void() +
    labs(
      title   = title,
      caption = if (!is.null(basemap) && basemap == "osm") "Basemap: OpenStreetMap contributors" else NULL
    ) +
    theme(
      plot.title        = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position   = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.box.background = element_rect(
        fill  = alpha("white", legend_background_alpha),
        color = "grey40", linewidth = 0.5
      ),
      legend.box.margin  = margin(5, 5, 5, 5, "pt"),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.title       = element_text(face = "bold", size = legend_title_size, lineheight = 1.2),
      legend.text        = element_text(size = legend_text_size),
      legend.key.size    = unit(legend_key_size, "cm"),
      legend.spacing.y   = unit(0.005, "cm"),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 10, 10, 10, "pt"),
      plot.caption       = element_text(hjust = 1, size = 7, color = "grey30", margin = margin(t = 5))
    )

  p
}

# Helper to save a map
save_map <- function(plot, name, dir = output_dir, w = 8, h = 7, dpi = 600) {
  path <- file.path(dir, paste0("map_", name, ".png"))
  ggsave(filename = path, plot = plot, width = w, height = h, dpi = dpi, bg = "white")
  cat("  Saved:", path, "\n")
}

# Common boundary args reused in every call
bd <- list(
  boundaries_sf      = ward_boundaries,
  boundary_color     = "grey20",
  boundary_width     = 0.6,
  boundary_label     = "Ward boundaries",
  add_ward_labels    = TRUE,
  ward_label_column  = "wardname",
  ward_label_size    = 3.5
)

# =============================================================================
# 1. Recycling — N recycled per maize hectare
# =============================================================================

map_data$final_N_total_per_ha2 <- round(map_data$final_N_total_per_ha, 0)

p <- do.call(create_discrete_map, c(list(
  data          = map_data,
  variable      = "final_N_total_per_ha2",
  legend_title  = "N (kg) / maize ha",
  n_classes     = 5, style = "jenks",
  palette       = "cividis", direction = -1
), bd))

save_map(p, "final_N_total_per_ha2")

# =============================================================================
# 2. Recycling — P recycled per maize hectare
# =============================================================================

map_data$final_total_P_per_ha2 <- round(map_data$final_P_total_per_ha, 1)

p <- do.call(create_discrete_map, c(list(
  data          = map_data,
  variable      = "final_total_P_per_ha2",
  legend_title  = "P (kg) / maize ha",
  n_classes     = 5, style = "jenks",
  palette       = "cividis", direction = -1
), bd))

save_map(p, "final_total_P_per_ha2")

# =============================================================================
# 3. Input use — Total N from legumes
# =============================================================================

map_data$total_N_legumes_tons <- round(map_data$total_N_legumes / 1000, 2)

p <- do.call(create_discrete_map, c(list(
  data          = map_data,
  variable      = "total_N_legumes_tons",
  legend_title  = "Total N legumes\n(tons)",
  n_classes     = 5, style = "jenks",
  palette       = "cividis", direction = -1
), bd))

save_map(p, "total_N_legumes_tons")

# =============================================================================
# 4. Biodiversity habitat score
# =============================================================================

map_data$biodiv_hab_raw <- round(map_data$biodiv_hab_raw, 2)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "biodiv_hab_raw",
  legend_title      = "Biodiv habitat\nscore",
  n_classes         = 6, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "MoMAColors::Alkalay2",
  direction         = 1
), bd))

save_map(p, "biodiv_hab_raw")

# =============================================================================
# 5. Soil organic carbon stocks
# =============================================================================

map_data$soc_stocks_ha <- round(map_data$soc_stocks / 500, 1)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "soc_stocks_ha",
  legend_title      = "SOC stocks\nT/ha (0-20cm)",
  n_classes         = 6, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "MexBrewer::Naturaleza",
  direction         = 1
), bd))

save_map(p, "soc_stocks_ha")

# =============================================================================
# 6. Synergy score
# =============================================================================

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "synergy_score_sum",
  legend_title      = "Synergy\nscore",
  n_classes         = 6, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "rcartocolor::BluYl",
  direction         = 1
), bd))

save_map(p, "synergy_score_sum")

# =============================================================================
# 7. Value chain connectivity (local food system orientation)
# =============================================================================

map_data$connectivity_score_brut_rd <- round(map_data$connectivity_score_brut, 1)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "connectivity_score_brut_rd",
  legend_title      = "Local Food System\nOrientation",
  n_classes         = 5, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "beyonce::X7",
  direction         = -1
), bd))

save_map(p, "connectivity_score_brut_rd")

# =============================================================================
# 8. Equity (reversed Gini)
# =============================================================================

map_data$equity_raw2 <- round(map_data$equity_raw, 2)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "equity_raw2",
  legend_title      = "Equity score\n(Reversed Gini)",
  n_classes         = 5, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "rcartocolor::Purp",
  direction         = 1
), bd))

save_map(p, "equity_raw2")

# =============================================================================
# 9. Economic diversity (Shannon index)
# =============================================================================

map_data$eco_div_shannon_raw <- round(map_data$eco_div_shannon_raw, 1)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "eco_div_shannon_raw",
  legend_title      = "Economic diversity\n(Shannon index)",
  n_classes         = 5, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "MoMAColors::Althoff",
  direction         = 1
), bd))

save_map(p, "eco_div_shannon_raw")

# =============================================================================
# 10. AE composite score
# =============================================================================

map_data$ae_composite_score2 <- round(map_data$ae_composite_score, 2)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "ae_composite_score2",
  legend_title      = "Agroecology\nScore",
  n_classes         = 6, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "LaCroixColoR::Orange",
  direction         = 1,
  alpha             = 0.65
), bd))

ggsave(
  filename = file.path(output_dir, "map_AE_composite_score.png"),
  plot = p, width = 8, height = 7, dpi = 600, bg = "white"
)
cat("  Saved:", file.path(output_dir, "map_AE_composite_score.png"), "\n")

cat("  All maps produced.\n")
