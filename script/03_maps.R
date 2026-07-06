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
# 7. Connectivity — composite score (orientation + accessibility)
# =============================================================================

map_data$connectivity_score_rd <- round(map_data$connectivity_score_brut, 2)

p <- do.call(create_discrete_map, c(list(
  data              = map_data,
  variable          = "connectivity_score_rd",
  legend_title      = "Local Food System\nConnectivity",
  n_classes         = 5, style = "jenks",
  use_paletteer     = TRUE,
  paletteer_palette = "beyonce::X7",
  direction         = -1
), bd))

save_map(p, "connectivity_score_composite")

# =============================================================================
# 7b. Connectivity — orientation sub-score
# =============================================================================

if ("orientation_score_brut" %in% colnames(map_data)) {
  map_data$orientation_score_rd <- round(map_data$orientation_score, 2)

  p <- do.call(create_discrete_map, c(list(
    data              = map_data,
    variable          = "orientation_score_rd",
    legend_title      = "Local Food System\nOrientation\n(subsistence vs. export)",
    n_classes         = 5, style = "jenks",
    use_paletteer     = TRUE,
    paletteer_palette = "beyonce::X7",
    direction         = -1
  ), bd))

  save_map(p, "connectivity_orientation_score")
}

# =============================================================================
# 7c. Connectivity — market accessibility sub-score
# =============================================================================

if ("accessibility_score_brut" %in% colnames(map_data)) {
  map_data$accessibility_score_rd <- round(map_data$accessibility_score, 2)

  p <- do.call(create_discrete_map, c(list(
    data              = map_data,
    variable          = "accessibility_score_rd",
    legend_title      = "Market Physical\nAccessibility\n(proximity score)",
    n_classes         = 5, style = "jenks",
    use_paletteer     = TRUE,
    paletteer_palette = "rcartocolor::Teal",
    direction         = -1
  ), bd))

  save_map(p, "connectivity_accessibility_score")
}

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





#### Summary statistics per idnicator


# Indicator agroecology composite ward level aggregation statistics
summary(map_data$ae_composite_score2)
# per wards, 



# Ensure CRS match and geometries are valid, then intersect and compute area-weighted means.
library(sf)
library(dplyr)
library(tidyverse)

# Recycling
summary(map_data$max_grazing_CC_TLU)
summary(map_data$final_TLU)
reduc_TLU <- map_data$final_TLU / map_data$max_grazing_CC_TLU
summary(reduc_TLU)
summary(map_data$final_N_manure_per_ha)
summary(map_data$N_litter_kg_per_maize_ha)

summary(map_data$final_N_total_per_ha)

### Summary P values
summary(map_data$final_P_total_per_ha)


# summary synergy
summary(map_data$synergy_score_sum)


# keep only needed columns from hex layer (add any other attributes you need)
hex_sf <- map_data %>%
  st_make_valid() %>%
  st_transform(st_crs(ward_boundaries)) %>%
  select(id, ae_composite_score, geometry = geometry)

wards_sf <- ward_boundaries %>%
  st_make_valid() %>%
  st_transform(st_crs(hex_sf)) %>%
  select(wardname, geometry = geometry)

# intersect (this actually cuts hexagons by ward boundaries)
hex_ward_pieces <- st_intersection(hex_sf, wards_sf)

# compute piece areas (numeric)
hex_ward_pieces <- hex_ward_pieces %>%
  mutate(piece_area = as.numeric(st_area(geometry)))

# area-weighted mean per ward (drop geometry for the table)
ward_ae_table <- hex_ward_pieces %>%
  st_set_geometry(NULL) %>%
  group_by(wardname) %>%
  summarize(
    mean_ae_composite_score = weighted.mean(
      ae_composite_score,
      w = piece_area,
      na.rm = TRUE
    ),
    total_area = sum(piece_area, na.rm = TRUE),
    n_hexagon_pieces = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ae_composite_score))

# add sd and n_hexagons per ward
ward_ae_table <- ward_ae_table %>%
  left_join(
    hex_ward_pieces %>%
      st_set_geometry(NULL) %>%
      group_by(wardname) %>%
      summarize(
        sd_ae_composite_score = sd(ae_composite_score, na.rm = TRUE),
        n_hexagons = n_distinct(id),
        .groups = "drop"
      ),
    by = "wardname"
  )


# join scores back to ward polygons (spatial result)
wards_with_scores <- wards_sf %>%
  left_join(ward_ae_table, by = "wardname")

# return a useful object
ward_scores_result <- list(table = ward_ae_table, wards_sf = wards_with_scores)
ward_scores_result

# print 5 top wards by score
print(head(ward_ae_table, 5))

#print 5 bottom wards by score
print(tail(ward_ae_table, 5))


# select top score of N_per_maize_ha from map_data
summary(map_data$final_N_total_per_ha)
top_N_values <- map_data %>%
  st_set_geometry(NULL) %>%
  arrange(desc(final_N_total_per_ha)) %>%
  slice_head(n = 5) %>%
  select(id, final_N_total_per_ha)

# use the id to bring the map_data lines and see the landscapes with top N values
top_N_landscapes <- map_data %>%
  filter(id %in% top_N_values$id) %>%
  select(id, final_N_total_per_ha, geometry)

#in map data, select the landsacpe var
str(map_data)


library(tidyverse)
library(sf)


# Get landscape with final_N_total_per_ha >70
top_N_values <- map_data %>%
  st_set_geometry(NULL) %>%
  filter(final_N_total_per_ha > 70) %>%
  select(id, final_N_total_per_ha)


# Select landscape composition variables for top 5
landscape_vars <- c(
  "wetland_grassland",
  "horticulture",
  "cropland",
  "mineral_bare_soil",
  "dense_woodland",
  "open_woodland",
  "grassland",
  "tree_hedges",
  "urban",
  "water"
)

top_landscapes_data <- map_data %>%
  st_set_geometry(NULL) %>%
  filter(id %in% top_N_values$id) %>%
  select(id, all_of(landscape_vars))

# Reshape data to long format
top_landscapes_long <- top_landscapes_data %>%
  pivot_longer(
    cols = -id,
    names_to = "landscape_type",
    values_to = "proportion"
  )

# Calculate mean and standard error for each landscape type
summary_stats <- top_landscapes_long %>%
  group_by(landscape_type) %>%
  summarise(
    mean_prop = mean(proportion),
    se = sd(proportion) / sqrt(n()),
    sd = sd(proportion)
  ) %>%
  mutate(landscape_type = factor(landscape_type, levels = landscape_vars))

# Create bar plot with error bars
ggplot(
  summary_stats,
  aes(x = reorder(landscape_type, -mean_prop), y = mean_prop)
) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(
    aes(ymin = mean_prop - se, ymax = mean_prop + se),
    width = 0.3,
    size = 0.8
  ) +
  labs(
    title = "Top Landscape Composition RECYCLING\n>70 Nkg/maize ha",
    # subtitle = nrow(top_N_values),
    subtitle = paste("Number of landscapes:", nrow(top_N_values)),
    x = "",
    y = "Mean Proportion ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )


# Print summary statistics
print(summary_stats)


# mean ae_composite score by ward and standard deviation
ward_ae_summary <- ward_ae_table %>%
  summarize(
    mean_ae_score = mean(mean_ae_composite_score, na.rm = TRUE),
    sd_ae_score = sd(mean_ae_composite_score, na.rm = TRUE),
    n_wards = n()
  )
print(ward_ae_summary)



#print the ward stat table all rows
print(ward_ae_table, n=24)
