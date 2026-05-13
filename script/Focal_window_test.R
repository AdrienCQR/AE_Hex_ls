# ==========================================
# 6. CRÉATION TABLE SCENARIOS FENÊTRE MOBILE
# ==========================================

library(terra)
library(dplyr)
library(tidyr)
library(glue)


# --- 1) Resampler le raster LULC à 30m avec fonction modale ---

lulc_rast <- rast("AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif")
legend_reclass <- read.csv2(
  "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv"
)

# Créer un template à 30m de résolution
lulc_template_30m <- rast(ext(lulc_rast), resolution = 30, crs = crs(lulc_rast))

# Resampler avec méthode modale (la plus fréquente)
lulc_30m <- resample(lulc_rast, lulc_template_30m, method = "mode")
plot(lulc_30m, main = "LULC resamplé à 30m (mode)")

# Sauvegarder le raster resampleé
writeRaster(
  lulc_30m,
  "AE_Hex_ls/data/lulc_processed/lulc_30m_resampled.tif",
  overwrite = TRUE
)

# table values of lulc_30m
table(values(lulc_30m))


library(terra)


# ==========================================
# CALCUL COMPOSITION LULC - VERSION OPTIMALE
# ==========================================

library(terra)
library(dplyr)

# Charger les données
lulc_30m <- rast("AE_Hex_ls/data/lulc_processed/lulc_30m_resampled.tif")
legend_reclass <- read.csv2(
  "AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.csv"
)

# Vérifier les valeurs présentes
print(table(values(lulc_30m)))
print(legend_reclass)


#sample area ward 28
# load communal ward shapes
wards <- vect("data/raw/communal_wards/communal_wards.shp")
# select ward 28 in wardname
ward_28 <- wards[wards$wardname == "28", ]
#plot ward 28
plot(ward_28)
# crop lulc_30m to ward 28
lulc_ward_28 <- crop(lulc_30m, ward_28)
# mask lulc_30m to ward 28
lulc_ward_28 <- mask(lulc_ward_28, ward_28)
#plot lulc_ward_28
plot(lulc_ward_28, main = "LULC in Ward 28")

# print res lulc_ward_28
res(lulc_ward_28)


# Classes présentes
classes_lulc <- unique(values(lulc_ward_28))
classes_lulc <- classes_lulc[!is.na(classes_lulc)]

# Fenêtre circulaire 5km
rayon_pixels <- round(5000 / res(lulc_ward_28)[1])

# Fonction pour créer fenêtre circulaire
make_circle_matrix <- function(radius) {
  size <- 2 * radius + 1
  m <- matrix(0, nrow = size, ncol = size)
  center <- radius + 1
  for (i in 1:size) {
    for (j in 1:size) {
      if (sqrt((i - center)^2 + (j - center)^2) <= radius) {
        m[i, j] <- 1
      }
    }
  }
  return(m)
}

window_circle <- make_circle_matrix(rayon_pixels)


prop_rasters <- list()

for (classe in classes_lulc) {
  cat(paste("  → Classe", classe, "\n"))

  # Raster binaire
  raster_binaire <- ifel(
    lulc_ward_28 == classe,
    1,
    ifel(is.na(lulc_ward_28), NA, 0)
  )

  # PROPORTION (mean)
  prop_raster <- focal(
    raster_binaire,
    w = window_circle,
    fun = "mean",
    na.rm = TRUE
  )

  names(prop_raster) <- paste0("prop_", classe)
  prop_rasters[[as.character(classe)]] <- prop_raster
}

# Stack toutes les proportions
stack_prop <- rast(prop_rasters)


# MÉTHODE 1 : Avec numéro de cellule (ID unique)
df_prop <- as.data.frame(stack_prop, xy = TRUE, cells = TRUE)

# La colonne "cell" est ton ID unique !
# cell = numéro de cellule dans le raster (de 1 à ncell(raster))

# Renommer proprement
col_names <- c("cell_id", "x", "y", paste0("prop_lulc_", classes_lulc))
names(df_prop) <- col_names

# Nettoyer les NA
df_prop_clean <- df_prop %>%
  filter(!is.na(prop_lulc_1)) # Ajuste selon ta première classe

cat(paste("  ✓", nrow(df_prop_clean), "pixels avec IDs\n"))

print(head(df_prop_clean))

# Vérifier que proportions somment à ~1
df_prop_clean$sum_check <- rowSums(
  df_prop_clean[, paste0("prop_lulc_", classes_lulc)],
  na.rm = TRUE
)

summary(df_prop_clean$sum_check)
