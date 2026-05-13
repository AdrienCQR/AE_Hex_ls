# clean environment
rm(list = ls())

# Packages
library(sf)
library(landscapemetrics)
library(dplyr)
library(ggplot2)
library(terra)
library(units)

# load data
# ward
ward <- vect("data/raw/communal_wards/communal_wards.shp")
# lulc
lulc <- rast("data/raw/lulc/V2_land_use_map_garden_32736.tif")
#load legend
legend <- read.csv2("data/raw/lulc/V2_land_use_map_codes.csv")
# plot map using class_name and color
# Set up the color scheme and labels
colors <- legend$color
colors
labels <- legend$class_name

str(legend)

# Create a proper categorical raster with class names
# First, set the categories/levels for the raster
levels(lulc) <- legend[, c("class_code", "class_name")]

# Create named color vector for the classes present in the raster
raster_values <- sort(unique(values(lulc, na.rm = TRUE)))
legend_subset <- legend[legend$class_code %in% raster_values, ]
legend_subset <- legend_subset[order(legend_subset$class_code), ]

# Plot with class names in legend
plot(
     lulc,
     type = "classes",
     col = legend_subset$color,
     main = "Land Use Land Cover Classification",
     axes = FALSE,
     plg = list(
          title = "LULC Classes",
          cex = 0.7,
          title.cex = 0.8
     )
)


# RECLASS TO HAVE MINEARL AND BARE SOIL IN ONE CLASS

# Create reclassification matrix
# Format: [from_value, to_value] for each row
reclass_matrix <- matrix(
     c(
          0,
          0, # dense_woodland stays 0
          1,
          1, # cropland stays 1
          2,
          2, # open_woodland stays 2
          3,
          3, # mineral becomes 3 (we'll keep this as the merged class)
          4,
          4, # urban stays 4
          5,
          3, # bare_soil becomes 3 (merged with mineral)
          6,
          6, # water stays 6
          7,
          7, # garden stays 7
          8,
          8, # grassland stays 8
          9,
          9 # wetland_grassland stays 9
     ),
     ncol = 2,
     byrow = TRUE
)

print("Reclassification matrix:")
print(reclass_matrix)

# Apply reclassification
lulc_reclass <- classify(lulc, reclass_matrix)

# Create new legend for the reclassified data
legend_new <- legend
# Change mineral class name to reflect the merge
legend_new$class_name[legend_new$class_code == 3] <- "mineral_bare_soil"
# Remove the bare_soil row since it's now merged
legend_new <- legend_new[legend_new$class_code != 5, ]

# Keep mineral color, or choose a new one:
legend_new$color[legend_new$class_code == 3] <- "#7f8183" # keeping mineral color
# Or use a new color: legend_new$color[legend_new$class_code == 3] <- "#a0a0a0"

print("New legend:")
print(legend_new)

# Set up the reclassified raster with new categories
levels(lulc_reclass) <- legend_new[, c("class_code", "class_name")]

# Get the classes present in reclassified raster
raster_values_new <- sort(unique(values(lulc_reclass, na.rm = TRUE)))
legend_subset_new <- legend_new[legend_new$class_code %in% raster_values_new, ]
legend_subset_new <- legend_subset_new[order(legend_subset_new$class_code), ]

# Plot the reclassified raster
plot(
     lulc_reclass,
     type = "classes",
     col = legend_subset_new$color,
     main = "Land Use Land Cover Classification (Mineral + Bare Soil Merged)",
     axes = FALSE,
     plg = list(
          title = "LULC Classes",
          cex = 0.7,
          title.cex = 0.8
     )
)


# save reclassified lulc
writeRaster(
     lulc_reclass,
     "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif",
     overwrite = TRUE
)
# Create the new legend for reclassified data
# Since you're merging class 5 (bare_soil) into class 3 (mineral),
# you'll have one less class total

legend_reclass <- data.frame(
     class_code = c(0, 1, 2, 3, 4, 6, 7, 8, 9),
     class_name = c(
          "dense_woodland",
          "cropland",
          "open_woodland",
          "mineral_bare_soil", # merged class
          "urban",
          "water",
          "horticulture",
          "grassland",
          "wetland_grassland"
     ),
     color = c(
          "#236832", # dense_woodland
          "#ffc767", # cropland
          "#39a751", # open_woodland
          "#7f8183", # mineral_bare_soil (keeping mineral color)
          "#fc8163", # urban
          "#2550ff", # water
          "#9b59b6", # garden
          "#d3ff6b", # grassland
          "#b0f0cf"
     ) # wetland_grassland
)

print("Reclassified legend:")
print(legend_reclass)

# Verify the order by comparing with original
print("\nOriginal legend for reference:")
print(legend)
write.csv2(
     legend_reclass,
     "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv",
     row.names = FALSE
)

# clean environment and reload
rm(list = ls())

# load reclassified lulc
lulc_reclass <- rast("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif")
# load reclassified legend
legend_reclass <- read.csv2(
     "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv"
)
# Plot the reclassified raster with new legend
plot(
     lulc_reclass,
     col = legend_reclass$color,
     main = "Reclassified LULC",
     axes = FALSE,
     plg = list(
          title = "LULC Classes",
          cex = 0.8
     )
)


## Make a grid

# Create hexagonal grid covering the entire lulc raster
# get bbox of lulc_reclass
bbox_lulc <- st_bbox(lulc_reclass)
# convert bbox to sf object
bbox_sf <- st_as_sfc(bbox_lulc)
## create hexagonal grid

# More precise hexagon sizing
target_area_km2 <- 5
hex_area_m2 <- target_area_km2 * 1000000

# Calculate side length for exact area
# Area formula: A = (3√3/2) × s²
hex_side_m <- sqrt(hex_area_m2 / (3 * sqrt(3) / 2))

# For st_make_grid with hexagons, cellsize = distance between centers
# Distance between centers = √3 × side_length
hex_cellsize <- sqrt(3) * hex_side_m


# Create hexagonal grid with correct cellsize
hex_grid <- st_make_grid(
     bbox_sf,
     cellsize = hex_cellsize, # Distance entre centres
     square = FALSE,
     what = "polygons"
)

# add an id column to the hex grid
hex_grid <- st_sf(id = seq_along(hex_grid), geometry = hex_grid)
# add area column for control
str(hex_grid)
hex_grid$area <- st_area(hex_grid)


# Convert to sf object
hex_grid_sf <- st_as_sf(hex_grid)
hex_grid_sf$area_km2 <- hex_grid_sf$area
hex_grid_sf$area_km2 <- set_units(hex_grid_sf$area_km2, km^2)


# Convert to terra object
hex_grid_terra <- vect(hex_grid_sf)
# Plot the hexagonal grid on top of lulc raster
plot(
     lulc_reclass,
     col = legend_reclass$color,
     main = "Reclassified LULC with Hexagonal Grid",
     axes = FALSE
)
plot(hex_grid_terra, add = TRUE, border = "red", lwd = 1)
# add id label
text(
     st_coordinates(st_centroid(hex_grid_sf)),
     labels = hex_grid_sf$id,
     cex = 0.5,
     col = "black"
)


# Masque binaire : 1 = données valides, NA = NoData
masque <- ifel(!is.na(lulc_reclass), 1, NA)
# plot raster
plot(
     masque,
     main = "Masque binaire des données valides",
     col = "lightblue",
     axes = FALSE
)
# Vectoriser le masque en un seul polygone (dissous)
masque_poly <- as.polygons(masque, dissolve = TRUE)
masque_sf <- st_as_sf(masque_poly) # Convertir en sf


# Vérifier si chaque hexagone est entièrement dans le masque
hex_complets <- st_within(hex_grid_sf, masque_sf, sparse = FALSE) %>%
     as.logical() %>% # Convertir en vecteur logique
     which() # Indices des hexagones valides

# Filtrer les hexagones
hex_filtres <- hex_grid_sf[hex_complets, ]
# convert to terra object
hex_filtres_terra <- vect(hex_filtres)


# plot les hexagones filtréson top of the raster
plot(
     lulc_reclass,
     col = legend_reclass$color,
     main = "Reclassified LULC with Valid Mask",
     axes = FALSE
)
plot(hex_filtres_terra, add = TRUE, border = "red", lwd = 1)

writeVector(
     hex_filtres_terra,
     "AE_Hex_ls/data/grid_tool/V2_grid_hex_5km2_filtered.shp",
     overwrite = TRUE
)


# Convert to terra object
hex_grid_terra <- vect(hex_grid_sf)

# Plot the hexagonal grid on top of lulc raster
plot(
     lulc_reclass,
     col = legend_reclass$color,
     main = "Reclassified LULC with Hexagonal Grid",
     axes = FALSE
)
plot(hex_grid_terra, add = TRUE, border = "red", lwd = 1)

# add id label
text(
     st_coordinates(st_centroid(hex_grid_sf)),
     labels = hex_grid_sf$id,
     cex = 0.5,
     col = "black"
)


masque_sf <- st_as_sf(masque_poly)

# ÉTAPE 2: Filtrer les hexagones entièrement dans le masque LULC
hex_complets_lulc <- st_within(hex_grid_sf, masque_sf, sparse = FALSE) %>%
     as.logical()

# Filtrer pour avoir seulement les hexagones avec couverture LULC complète
hex_lulc_complets <- hex_grid_sf[hex_complets_lulc, ]

print(paste(
     "Hexagones avec couverture LULC complète:",
     nrow(hex_lulc_complets)
))

# ÉTAPE 3: Lire et préparer les données ward
ward <- st_read("data/raw/communal_wards/communal_wards.shp")

# Vérifier et corriger les géométries invalides
ward$geometry <- st_make_valid(ward$geometry)

# ÉTAPE 4: Filtrer les hexagones qui INTERSECTENT avec les ward
# Utiliser st_intersects au lieu de st_within
hex_intersect_ward <- st_intersects(hex_lulc_complets, ward, sparse = FALSE) %>%
     apply(1, any) # TRUE si l'hexagone intersecte au moins un ward

# Filtrer les hexagones finaux
hex_filtres_final <- hex_lulc_complets[hex_intersect_ward, ]

print(paste(
     "Hexagones finaux (LULC complet + intersectent ward):",
     nrow(hex_filtres_final)
))

# Convert to terra object
hex_filtres_final_terra <- vect(hex_filtres_final)

# PLOT FINAL
plot(
     lulc_reclass,
     col = legend_reclass$color,
     main = "Hexagones filtrés: LULC complet + Intersection Ward",
     axes = FALSE
)

# Ajouter les ward en arrière-plan
plot(st_geometry(ward), add = TRUE, border = "blue", lwd = 0.5, col = NA)

# Ajouter les hexagones filtrés
plot(hex_filtres_final_terra, add = TRUE, border = "red", lwd = 2)

# Ajouter les labels
text(
     st_coordinates(st_centroid(hex_filtres_final)),
     labels = hex_filtres_final$id,
     cex = 0.6,
     col = "black",
     font = 2
)

# Sauvegarder le résultat
writeVector(
     hex_filtres_final_terra,
     "AE_Hex_ls/data/grid_tool/V2_grid_hex_5km2_filtered_final.shp",
     overwrite = TRUE
)
