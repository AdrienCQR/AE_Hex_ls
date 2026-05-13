
# CONNECTIVITY_V2.R — VERSION TEST (Ward 28 uniquement)
# Composite Local Food System Connectivity Score
# = 0.5 * OrientationScore + 0.5 * AccessibilityScore

# ============================================================
# 0. PACKAGES
# ============================================================
library(terra)
library(sf)
library(tidyverse)
library(osmdata)
library(gdistance)
library(exactextractr)
library(ggmap)

rm(list = ls())
# ============================================================
# 1. USER INPUTS
# ============================================================
study_area_path <- "data/raw/communal_wards/communal_wards.shp"
lulc_path       <- "data/raw/lulc/V2_land_use_map_garden_32736.tif"
grid_path       <- "data/grid_tool/V4_grid_hex_5km2_filtered_final.shp"
market_source   <- "both"  # "marketplace", "towns", "both"
alpha           <- 0.5
osm_buffer_km   <- 70

road_speeds <- c(
  trunk        = 100,
  primary      = 100,
  secondary    = 80,
  tertiary     = 30,
  unclassified = 10,
  service      = 10,
  track        = 10,
  residential  = 10
)

# ============================================================
# 2. CHARGEMENT + FILTRE WARD 28 (TEST)
# ============================================================
cat(">> Chargement et filtre Ward 28...\n")

study_area <- st_read(study_area_path, quiet = TRUE)
lulc       <- rast(lulc_path)
grid       <- st_read(grid_path, quiet = TRUE)

ward28      <- study_area %>% filter(wardname == "28")
crs_project <- crs(lulc)
ward28_proj <- st_transform(ward28, crs_project)

# Buffer élargi pour requête OSM (en mètres via Web Mercator)
ward28_buffered  <- st_buffer(st_transform(ward28, 3857), dist = osm_buffer_km * 1000)
bbox_osm         <- st_bbox(st_transform(ward28_buffered, 4326))

cat("   Buffer OSM :", osm_buffer_km, "km — Bbox :", round(as.numeric(bbox_osm), 3), "\n")

# Crop LULC sur ward28
lulc_ward28 <- crop(lulc, vect(ward28_proj)) %>% mask(vect(ward28_proj))
names(lulc_ward28) <- "lulc"

# Filtre grid
grid_ward28 <- st_filter(st_transform(grid, crs_project), ward28_proj)
grid_ward28 <- grid_ward28[
  !st_is_empty(st_intersection(grid_ward28, st_as_sf(st_buffer(ward28_proj, -100)))), 
]
grid_ward28$cell_id <- seq_len(nrow(grid_ward28))

cat("   Cellules :", nrow(grid_ward28), "\n")

plot(lulc_ward28, main = "Ward 28 — LULC + grid")
plot(st_geometry(grid_ward28), add = TRUE, border = "red", lwd = 1)

# ============================================================
# 3. POINTS MARCHÉS
# ============================================================
cat(">> Téléchargement points marchés OSM...\n")

get_market_points <- function(bbox, source) {
  pts_list <- list()

  if (source %in% c("marketplace", "both")) {
    q <- opq(bbox) %>% add_osm_feature(key = "amenity", value = "marketplace") %>% osmdata_sf()
    if (!is.null(q$osm_points)   && nrow(q$osm_points) > 0)
      pts_list[["mp_pts"]]  <- q$osm_points %>% dplyr::select(geometry)
    if (!is.null(q$osm_polygons) && nrow(q$osm_polygons) > 0)
      pts_list[["mp_poly"]] <- st_centroid(q$osm_polygons) %>% dplyr::select(geometry)
  }

  if (source %in% c("towns", "both")) {
    q <- opq(bbox) %>% add_osm_feature(key = "place", value = c("town", "city")) %>% osmdata_sf()
    if (!is.null(q$osm_points) && nrow(q$osm_points) > 0)
      pts_list[["towns"]] <- q$osm_points %>% dplyr::select(geometry)
  }

  if (length(pts_list) == 0) {
    warning("Aucun marché/ville trouvé — fallback sur villages.")
    q <- opq(bbox) %>% add_osm_feature(key = "place", value = c("town", "city", "village")) %>% osmdata_sf()
    if (!is.null(q$osm_points) && nrow(q$osm_points) > 0)
      pts_list[["fallback"]] <- q$osm_points %>% dplyr::select(geometry)
  }

  bind_rows(pts_list)
}

markets_osm  <- get_market_points(bbox_osm, source = market_source)
markets_proj <- st_transform(markets_osm, crs_project)

cat("   Points OSM trouvés :", nrow(markets_proj), "\n")

# save markets_proj
st_write(
  markets_proj,
  "data/processed/market_points_osm.shp",
  delete_layer = TRUE
)

# ------------------------------------------------------------
# Chargement du fichier de points manuels
# ------------------------------------------------------------
# /!\ IMPORTANT — VÉRIFICATION MANUELLE REQUISE
# Avant de continuer, vérifiez que tous les marchés/villes importants
# de votre zone sont bien présents dans le plot ci-dessous.
# Si des points manquent (ex: ville non répertoriée sur OSM) :
#   1. Ouvrez le fichier : data/raw/manual_market_points_buffered.shp dans QGIS
#   2. Ajoutez les points manquants manuellement
#   3. Sauvegardez et relancez à partir d'ici
# ------------------------------------------------------------

markets_manual <- st_read("data/processed/manual_market_points_buffered.shp", quiet = TRUE) %>%
  st_transform(crs_project) %>%
  dplyr::select(geometry)

cat("   Points manuels chargés :", nrow(markets_manual), "\n")

# keep only the manual points layer
markets_proj <- markets_manual

# plot market points first, then ward boundary
plot(st_geometry(markets_proj), main = "Points marchés (OSM + manuels)", pch = 16, col = "blue")
plot(st_geometry(ward28_proj), add = TRUE, border = "red", lwd = 2)




# --- 3b. Réseau routier --- (bbox_osm à la place de bbox_wgs84)
cat(">> Téléchargement réseau routier OSM...\n")

roads_osm <- opq(bbox_osm) %>%        # <-- bbox élargie ici aussi
  add_osm_feature(key = "highway", value = names(road_speeds)) %>%
  osmdata_sf()


roads_sf <- roads_osm$osm_lines %>%
  dplyr::select(highway) %>%
  filter(highway %in% names(road_speeds))


roads_proj           <- st_transform(roads_sf, crs_project)

roads_proj$speed_kmh <- road_speeds[roads_proj$highway]

cat("   Segments routiers dans le buffer :", nrow(roads_proj), "\n")

#make a table with the count of each highway type
print(table(roads_proj$highway))
#save roads_proj as shapefile
st_write(
  roads_proj,
  "data/processed/roads_osm.shp",
  delete_layer = TRUE
)

# reload roads_proj as sf object
roads_proj <- st_read("data/processed/roads_osm.shp", quiet = TRUE) %>%
  st_transform(crs_project)


# ============================================================
# 4. RASTER DE FRICTION — sur l'emprise élargie, résolution 50m
# ============================================================
cat(">> Création du raster de friction (résolution 50m)...\n")

res_friction_m    <- 50  # <-- paramètre à ajuster si besoin (30, 50, 100)
walking_speed_kmh <- 5

ward28_buf_proj <- st_transform(ward28_buffered, crs_project)

# Template à 50m sur l'emprise élargie
rast_template <- rast(
  ext(vect(ward28_buf_proj)),
  resolution = res_friction_m,
  crs        = crs_project
)

# Rasterisation du réseau routier
roads_rast <- rasterize(vect(roads_proj), rast_template, field = "speed_kmh")

# Friction = temps (heures) pour traverser un pixel
friction <- ifel(
  is.na(roads_rast),
  res_friction_m / (walking_speed_kmh * 1000),  # hors route : à pied
  res_friction_m / (roads_rast * 1000)           # sur route : vitesse OSM
)
names(friction) <- "friction"
#plot raster continue
plot(friction, main = "Raster de friction (heures pour traverser un pixel)")

# write friction raster
writeRaster(
  friction,
  "data/processed/friction_raster.tif",
  overwrite = TRUE
)




# ============================================================
# 5. RASTER DE TEMPS D'ACCÈS — via terra::costDist (plus rapide)
# ============================================================
cat(">> Calcul du raster de temps d'accès (terra)...\n")

# Rasteriser les points marchés — on veut juste un raster binaire (présence/absence)
markets_rast <- rasterize(vect(markets_proj), friction, fun = "count")
markets_rast <- ifel(markets_rast > 0, 1, NA)  # binaire : 1 = marché, NA = ailleurs

plot(markets_rast, main = "Points marchés rasterisés")

# table with count of market pixels
print(table(markets_rast[]))
# Alternative — passer directement les coordonnées comme origine
markets_coords <- st_coordinates(markets_proj)


# costDist attend :
# - target = valeur des cellules sources (défaut = 0)
# - le raster x contient la friction, avec 0 aux emplacements des marchés

friction <- ifel(
  is.na(roads_rast),
  1 / (walking_speed_kmh * 1000),   # hors route : 1 / (5 km/h → m/h)
  1 / (roads_rast * 1000)            # sur route  : 1 / (vitesse en m/h)
)
names(friction) <- "friction"

# Puis recalcul
friction_with_markets <- friction
friction_with_markets[markets_rast == 1] <- 0

time_access_r <- terra::costDist(friction_with_markets, target = 0)
names(time_access_r) <- "time_hours"

summary(values(time_access_r, na.rm = TRUE))

# plot du raster de temps d'accès
plot(time_access_r, main = "Raster de temps d'accès (heures)")
#plot markets
plot(st_geometry(markets_proj), add = TRUE, pch = 16, col = "blue")

# save time_access_r
writeRaster(
  time_access_r,
  "data/processed/time_access_raster.tif",
  overwrite = TRUE
)



# ============================================================
# 6. EXTRACTION PAR MAILLE × LAND USE — pixels bruts
# ============================================================
cat(">> Extraction temps d'accès par pixel...\n")

time_access_resampled <- resample(time_access_r, lulc_ward28, method = "bilinear")
stack_r <- c(lulc_ward28, time_access_resampled)

grid_ward28$cell_id <- seq_len(nrow(grid_ward28))

# Extraction pixel par pixel — pas d'agrégation
pixels_df <- exact_extract(stack_r, grid_ward28,
                            include_cols = "cell_id",
                            include_xy   = TRUE)

pixels_df <- bind_rows(pixels_df) %>%
  filter(!is.na(lulc), !is.na(time_hours))

pixels_df$ward <- "28"

cat("   Nombre de pixels extraits :", nrow(pixels_df), "\n")

# ============================================================
# 6b. AGRÉGATION PAR MAILLE × LAND USE (pour le score)
# Médiane comme estimateur central — robuste aux pixels extrêmes
# IQR conservé pour analyses exploratoires (inégalité d'accès intra-maille)
# ============================================================
time_by_lulc <- pixels_df %>%
  group_by(cell_id, lulc) %>%
  summarise(
    median_time = median(time_hours, na.rm = TRUE),
    mean_time   = weighted.mean(time_hours, coverage_fraction, na.rm = TRUE),
    iqr_time    = IQR(time_hours, na.rm = TRUE),
    area_ha     = sum(coverage_fraction, na.rm = TRUE) *
                  prod(res(stack_r)) / 10000,
    .groups = "drop"
  )

head(time_by_lulc)

# ============================================================
# BOXPLOTS EXPLORATOIRES
# ============================================================

# Charger les noms LULC si dispo
lulc_names <- read.csv2("data/lulc_processed/V2_lulc_reclassified.csv")
colnames(lulc_names) <- c("lulc", "class_name", "color")
pixels_df <- left_join(pixels_df, lulc_names, by = "lulc")

# Boxplot global — ward entier
ggplot(pixels_df, aes(x = as.factor(lulc), y = time_hours)) +
  geom_boxplot(outlier.size = 0.5, fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Temps d'accès au marché par classe LULC — Ward 28",
    x     = "Classe LULC",
    y     = "Temps d'accès (heures)"
  ) +
  theme_bw()

# Boxplot par maille × classe LULC
ggplot(pixels_df, aes(x = as.factor(lulc), y = time_hours)) +
  geom_boxplot(outlier.size = 0.3, fill = "steelblue", alpha = 0.6) +
  facet_wrap(~cell_id) +
  labs(
    title = "Temps d'accès par classe LULC et par maille",
    x     = "Classe LULC",
    y     = "Temps d'accès (heures)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot IQR par maille — visualise l'inégalité d'accès intra-maille
ggplot(time_by_lulc, aes(x = as.factor(lulc), y = iqr_time)) +
  geom_boxplot(fill = "coral", alpha = 0.6) +
  labs(
    title = "Variabilité intra-maille du temps d'accès (IQR) par classe LULC",
    x     = "Classe LULC",
    y     = "IQR temps d'accès (heures)"
  ) +
  theme_bw()



# ============================================================
# 7. CALCUL DE L'ACCESSIBILITY SCORE BRUT PAR MAILLE
# ============================================================
cat(">> Calcul de l'AccessibilityScore brut...\n")

# Score brut = médiane du temps pondérée par l'aire de chaque classe LULC
# Valeur en heures — sera normalisé plus tard avec les autres indicateurs
accessibility_raw <- time_by_lulc %>%
  group_by(cell_id) %>%
  summarise(
    accessibility_score_brut = weighted.mean(median_time, area_ha, na.rm = TRUE),
    .groups = "drop"
  )

cat("   Résumé accessibility_score_brut (heures) :\n")
print(summary(accessibility_raw$accessibility_score_brut))

# Jointure sur le grid
grid_final <- grid_ward28 %>%
  left_join(
    accessibility_raw %>% dplyr::select(cell_id, accessibility_score_brut),
    by = "cell_id"
  )

# Visualisation
# reset plot margins
plot(grid_final["accessibility_score_brut"],
     main   = "Accessibility score brut (heures)",
     breaks = "quantile", nbreaks = 5)

# Sauvegarde des outputs de ce script
st_write(
  grid_final,
  "data/processed/connectivity_v2_ward28.shp",
  delete_layer = TRUE
)

write.csv(pixels_df,    "data/processed/pixels_time_lulc_ward28.csv",  row.names = FALSE)
write.csv(time_by_lulc, "data/processed/time_by_lulc_ward28.csv",      row.names = FALSE)

cat(">> Fichiers sauvegardés :\n")
cat("   - data/processed/connectivity_v2_ward28.shp  (grid avec accessibility_score_brut)\n")
cat("   - data/processed/pixels_time_lulc_ward28.csv (pixels bruts pour boxplots)\n")
cat("   - data/processed/time_by_lulc_ward28.csv     (médiane/IQR par maille x classe)\n")
cat(">> Script CONNECTIVITY_V2 terminé.\n")

# ============================================================
# ÉTAPES SUIVANTES — CONNECTIVITY_V3.R (score composite final)
# ============================================================
#
#   Score composite connectivity indicator final
#   connectivity_score = alpha * orientation_score + (1 - alpha) * accessibility_score
#
