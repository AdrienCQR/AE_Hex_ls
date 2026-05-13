## Load data and tools
rm(list = ls())
# Packages
library(sf)
library(landscapemetrics)
library(dplyr)
library(ggplot2)
library(terra)
library(units)
library(exactextractr)

# grid
hex_grid <- st_read(
  "AE_Hex_ls/data/grid_tool/V2_grid_hex_5km2_filtered_final.shp"
)
hex_grid_terra <- vect(hex_grid)
lulc_rast <- rast("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif")
legend_reclass <- read.csv2(
  "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv"
)

# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask",
  axes = FALSE
)
plot(hex_grid_terra, add = TRUE, border = "red", lwd = 1)


### Filter grid to remove town area (id =441, 459, 460, 477)
str(hex_grid)


# Filtrer la grille pour enlever les zones urbaines (id = 441, 459, 460, 477)
hex_grid_filtered <- hex_grid[!hex_grid$id %in% c(441, 459, 460, 477), ]
hex_grid_filtered_terra <- vect(hex_grid_filtered)
# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask",
  axes = FALSE
)
plot(hex_grid_filtered_terra, add = TRUE, border = "red", lwd = 1)


# 1 make df scenario strucutre
df_raw <- read.csv("data/raw/10k_scenarios_realistic_compo_V5.csv")
str(df_raw)
df_compo <- df_raw[0, ]
str(df_compo)
# 2 extract hex compo into the df landscape compo

# Fonction pour extraire les proportions LULC avec renommage direct
extract_lulc_proportions <- function(lulc_rast, hex_grid, legend_reclass) {
  # Extraire les proportions pour chaque maille de la grille
  proportions <- exact_extract(lulc_rast, hex_grid, 'frac')

  # Convertir en data.frame et ajouter les IDs
  df_proportions <- data.frame(proportions)
  df_proportions$scenario_id <- hex_grid$id

  # Réorganiser les colonnes pour mettre scenario_id en premier
  df_proportions <- df_proportions[, c(
    "scenario_id",
    setdiff(names(df_proportions), "scenario_id")
  )]

  # Renommer les colonnes selon legend_reclass
  for (i in 1:nrow(legend_reclass)) {
    old_name <- paste0("frac_", legend_reclass$class_code[i])
    new_name <- legend_reclass$class_name[i]

    if (old_name %in% names(df_proportions)) {
      names(df_proportions)[names(df_proportions) == old_name] <- new_name
    }
  }

  # Créer le tableau final avec la structure souhaitée
  df_final <- data.frame(
    scenario_id = df_proportions$scenario_id,
    wetland_grassland = if ("wetland_grassland" %in% names(df_proportions)) {
      df_proportions$wetland_grassland
    } else {
      0
    },
    horticulture = if ("horticulture" %in% names(df_proportions)) {
      df_proportions$horticulture
    } else {
      0
    },
    cropland = if ("cropland" %in% names(df_proportions)) {
      df_proportions$cropland
    } else {
      0
    },
    mineral_bare_soil = if ("mineral_bare_soil" %in% names(df_proportions)) {
      df_proportions$mineral_bare_soil
    } else {
      0
    },
    dense_woodland = if ("dense_woodland" %in% names(df_proportions)) {
      df_proportions$dense_woodland
    } else {
      0
    },
    open_woodland = if ("open_woodland" %in% names(df_proportions)) {
      df_proportions$open_woodland
    } else {
      0
    },
    grassland = if ("grassland" %in% names(df_proportions)) {
      df_proportions$grassland
    } else {
      0
    },
    urban = if ("urban" %in% names(df_proportions)) df_proportions$urban else 0,
    water = if ("water" %in% names(df_proportions)) df_proportions$water else 0
  )

  # Remplacer les NA par 0
  df_final[is.na(df_final)] <- 0

  return(df_final)
}

# Utilisation
df_compo <- extract_lulc_proportions(
  lulc_rast,
  hex_grid_filtered,
  legend_reclass
)

# Vérifier la structure
str(df_compo)
summary(df_compo)
df_compo$scenario_id <- as.character(df_compo$scenario_id)

str(df_compo)


write.csv(
  df_compo,
  "AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v3.csv",
  row.names = FALSE
)
# Sauvegarder la grille filtrée
writeVector(
  hex_grid_filtered_terra,
  "AE_Hex_ls/data/grid_tool/V3_grid_hex_5km2_town_filtered.shp",
  overwrite = TRUE
)


scenario_compo <- read.csv(
  "AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v3.csv"
)

summary(scenario_compo)
str(scenario_compo)

# reload the hex_grid_filtered_terra
hex_grid_filtered <- st_read(
  "AE_Hex_ls/data/grid_tool/V3_grid_hex_5km2_town_filtered.shp"
)


# plot hex grids with less than 10% cropland
hex_grid_filtered_10 <- hex_grid_filtered[scenario_compo$cropland < 0.1, ]
hex_grid_filtered_10_terra <- vect(hex_grid_filtered_10)
# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask and <10% Cropland",
  axes = FALSE
)
plot(hex_grid_filtered_10_terra, add = TRUE, border = "blue", lwd = 1)

# plot hex grids with less than 15% cropland
hex_grid_filtered_15 <- hex_grid_filtered[scenario_compo$cropland < 0.15, ]
hex_grid_filtered_15_terra <- vect(hex_grid_filtered_15)
# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask and <15% Cropland",
  axes = FALSE
)
plot(hex_grid_filtered_15_terra, add = TRUE, border = "blue", lwd = 1)


# plot hex grid with more than 30% mineral bare soil
hex_grid_filtered_30 <- hex_grid_filtered[
  scenario_compo$mineral_bare_soil > 0.3,
]
hex_grid_filtered_30_terra <- vect(hex_grid_filtered_30)
# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask and >30% Mineral Bare Soil",
  axes = FALSE
)
plot(hex_grid_filtered_30_terra, add = TRUE, border = "blue", lwd = 1)


#select hex with more than 10 % croland and less than 30% mineral bare soil
hex_grid_filtered_final <- hex_grid_filtered[
  scenario_compo$cropland >= 0.1 & scenario_compo$mineral_bare_soil <= 0.3,
]
hex_grid_filtered_final_terra <- vect(hex_grid_filtered_final)
# plot les hexagones filtréson top of the raster
plot(
  lulc_rast,
  col = legend_reclass$color,
  main = "Reclassified LULC with Valid Mask and >10% Cropland & <30% Mineral Bare Soil",
  axes = FALSE
)
plot(hex_grid_filtered_final_terra, add = TRUE, border = "blue", lwd = 1)


#save the final filtered hex grid called V2_grid_hex_5km2_filtered_final.shp
writeVector(
  hex_grid_filtered_final_terra,
  "AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp",
  overwrite = TRUE
)
# save the final filtered scenario composition
df_compo_final <- df_compo[
  df_compo$scenario_id %in% hex_grid_filtered_final$id,
]
write.csv(
  df_compo_final,
  "AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv",
  row.names = FALSE
)
