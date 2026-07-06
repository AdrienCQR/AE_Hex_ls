# 00_prep_tree_hedge_fractions.R
# =============================================================================
# One-off preparation script — NOT part of the main pipeline (RUN_MAIN.R).
#
# Purpose:
#   Cross the LULC map (5 m, PlanetScope) with the tree cover map
#   (1 m, Brandt et al. / Reiner et al., 2023) to estimate the proportion of
#   tree cover (hedgerows, scattered trees) hidden within the cropland and
#   grassland LULC classes.
#
#   Run this script ONCE to derive `tree_hedge_cropland` and
#   `tree_hedge_grassland`. The resulting percentages are then hardcoded as
#   fixed parameters in `00_config.R` and applied uniformly to every hexagon
#   by `indicators/tree_hedges_processing.R`. No need to re-run unless the
#   source rasters change.
# =============================================================================

rm(list = ls())
library(terra)

# Load LULC map (5 m resolution)
lulc_2023 <- rast("data/raw/lulc/land_use_map_32736.tif")
lulc_legend <- read.csv2("data/raw/lulc/land_use_map_codes.csv", stringsAsFactors = FALSE)

# Load tree cover map (1 m resolution, Brandt et al. / Reiner et al., 2023)
tree_cover <- rast("data/raw/lulc/tree_cover_brandt_2019_epsg_32736.tif")

plot(tree_cover)
plot(lulc_2023, col = lulc_legend$color[lulc_legend$class_code], main = "LULC 2023")

communal_wards <- vect("data/raw/communal_wards/communal_wards.shp")

# Restrict analysis to wards 26, 27, 28
target_wards <- communal_wards[communal_wards$wardname %in% c(26, 27, 28), ]

print("Selected wards:")
print(target_wards[, c("wardpc", "wardname", "area_sqkm")])

plot(communal_wards, col = "lightgray", main = "Murehwa")
plot(target_wards, col = "red", add = TRUE)
text(target_wards, "wardname", cex = 0.8)

# Crop LULC and tree cover to the 3 wards
lulc_study <- crop(lulc_2023, target_wards)
lulc_study <- mask(lulc_study, target_wards)

tree_study <- crop(tree_cover, target_wards)
tree_study <- mask(tree_study, target_wards)

plot(lulc_study, col = lulc_legend$color[lulc_legend$class_code], main = "LULC Study Area")
plot(tree_study, col = c("white", "green"), main = "Tree Cover Study Area")  # 1 = tree cover, 0 = no tree cover

# tree_cover is finer (~1m) than LULC (~4.5m)
# Resample LULC (nearest neighbour) onto the fine tree_cover grid to keep full precision
lulc_fine <- resample(lulc_study, tree_study, method = "near")
lulc_final <- lulc_fine

# Build a stack of the aligned rasters
raster_stack <- c(lulc_final, tree_study)
names(raster_stack) <- c("lulc", "tree_cover")

print("Stack created:")
print(raster_stack)

# Extract the combined values in a single operation
stack_values <- values(raster_stack, na.rm = TRUE)
head(stack_values)

# Build a data.frame for the cross-analysis
df_analysis <- data.frame(
  lulc = stack_values[, "lulc"],
  tree_cover = stack_values[, "tree_cover"]
)
head(df_analysis)

print("Data structure:")
str(df_analysis)

print("Unique tree_cover values:")
unique(df_analysis$tree_cover)

print("Unique lulc values:")
unique(df_analysis$lulc)

print("tree_cover distribution:")
table(df_analysis$tree_cover)

# Tree cover statistics per LULC class
tree_stats_correct <- df_analysis %>%
  group_by(lulc) %>%
  summarise(
    total_pixels = n(),
    pixels_with_trees = sum(tree_cover == 1, na.rm = TRUE),
    pixels_without_trees = sum(tree_cover == 0, na.rm = TRUE),
    pct_tree_cover = (pixels_with_trees / total_pixels) * 100,
    area_total_ha = total_pixels * 1 / 10000  # 1m2 per pixel -> hectares
  ) %>%
  arrange(desc(pct_tree_cover))

print("Tree cover statistics:")
print(tree_stats_correct)

# Add class names
tree_stats_final <- tree_stats_correct %>%
  left_join(lulc_legend, by = c("lulc" = "class_code"))

print("With class names:")
print(tree_stats_final)

# Focus on cropland and grassland: these percentages feed
# tree_hedge_cropland / tree_hedge_grassland in 00_config.R
crop_grass_correct <- tree_stats_final %>%
  filter(class_name %in% c("cropland", "grassland")) %>%
  select(class_name, area_total_ha, total_pixels, pixels_with_trees, pct_tree_cover)

print("CROPLAND vs GRASSLAND:")
print(crop_grass_correct)

library(ggplot2)
library(dplyr)

# Create the plot with tree_stats_final data
tree_cover_plot <- ggplot(tree_stats_final, aes(x = reorder(class_name, pct_tree_cover), 
                                                y = pct_tree_cover, 
                                                fill = color)) +
  geom_col(width = 0.7) +
  scale_fill_identity() +  # Use exact colors from the table
  coord_flip() +  # Horizontal bars
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     breaks = seq(0, 50, by = 10),
                     expand = c(0, 0)) +
  labs(
    title = "Tree Cover by LULC Class",
    subtitle = "Percentage of pixels with\ntree presence by LULC class",
    x = "",
    y = "Tree Cover (%)",
    caption = "Source: Remote Sensing post-treatment - LULC Classification"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0),
    axis.text.y = element_text(size = 11, color = "#2c3e50"),
    axis.text.x = element_text(size = 10, color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ecf0f1", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Adaptive label placement based on bar length
  geom_text(aes(label = paste0(round(pct_tree_cover, 1), "%"),
                hjust = ifelse(pct_tree_cover > 35, 1.1, -0.1),  # Inside the bar if >35%
                color = ifelse(pct_tree_cover > 35, "white", "#2c3e50")), # White if inside
            size = 3.5,
            fontface = "bold") +
  scale_color_identity()  # Use the defined colors

# Display the plot
print(tree_cover_plot)


# Second plot: total area per class, as a complement
tree_area_plot <- ggplot(tree_stats_final, aes(x = reorder(class_name, area_total_ha), 
                                               y = area_total_ha/1000, 
                                               fill = color)) +
  geom_col(width = 0.7) +
  scale_fill_identity() +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "k"), 
                     expand = c(0, 0)) +
  labs(
    title = "Total Area by Land Cover Class",
    subtitle = "Area in thousands of hectares",
    x = "Land Cover Class",
    y = "Area (thousand ha)",
    caption = "Source: Remote sensing analysis - LULC classification"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0),
    axis.text.y = element_text(size = 11, color = "#2c3e50"),
    axis.text.x = element_text(size = 10, color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ecf0f1", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  geom_text(aes(label = paste0(round(area_total_ha/1000, 0), "k ha")), 
            hjust = -0.1, 
            size = 3.5, 
            color = "#2c3e50",
            fontface = "bold")

print(tree_area_plot)











