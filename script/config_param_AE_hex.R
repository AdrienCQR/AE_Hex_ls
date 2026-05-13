# config_parameters.R
# ===================
# ⚙️  PARAMÈTRES CALCUL DES INDICATEURS
# ===================

# Version et date
VERSION <- "V1.0"


# ===================
# 📁 FICHIERS
# ===================
FILE_PATHS <- list(
  input_scenarios = "AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_v2.csv",
  output_results = paste0(
    "outputs/tables/results_",
    VERSION,
    "_",
    Sys.Date(),
    ".csv"
  )
)

OUTPUT_DIR <- "outputs/tables/"

# ===================
# 🔘 INDICATEURS À CALCULER (TRUE/FALSE)
# ===================
INDICATORS_TO_RUN <- list(
  SOIL_HEALTH = TRUE,
  BIODIV = TRUE,
  WOOD = TRUE,
  N_LITTER_TREES = TRUE,
  N_LEGUMES_ROTATION = TRUE,
  GRAZING_CC = TRUE,
  AGRO_PASTO_LOOP = TRUE,
  ECO_DIV = TRUE,
  FAIRNESS = TRUE,
  FSS = FALSE,
  FOOD_ECONOMIC_SECURITY = FALSE,
  FOOD_SECURITY = FALSE,
  VULNERABILITY = FALSE,
  NUTRITION_DIVERSITY = FALSE,
  CONNECTIVITY_VAL_CHAIN = TRUE,
  SYNERGY = TRUE,
  AE_COMPOSITE = TRUE,
  FSN_COMPOSITE = FALSE
)

# ===================
# 📊 PARAMÈTRES DES INDICATEURS
# ===================

## Landscape total area
TOTAL_AREA_HA <- 500 # hex area

#### Parameters
R <- 832 # Rain (editable)

maize_share <- 0.74 # maize current share in cropland (Manyanga et al., 2025)
legume_share <- 0.21 # Legume share in rotation (Manyanga et al., 2025)
tobacco_share <- 0.05 # Tobacco share in cropland (Manyanga et al., 2025)
sum(legume_share, maize_share, tobacco_share) # should be 1.0
check_share <- (maize_share + legume_share + tobacco_share) # Check total share = 1


# Pourcentage de couvert arboré dans chaque classe (valeurs baseline pour le mom)
tree_hedge_cropland <- 0.059 # 5.9% de couvert arboré dans cropland
tree_hedge_grassland <- 0.034 # 3.4% de couvert arboré dans grassland


collectable_litter_per_HH <- 1.25 # Collectable litter in tons / household / year (ranging from 0.5 to 2 tons ?)
pop_density <- 71 # mean inhabitant / km² in rural Communal wards (without Murehwa town)
total_pop <- pop_density * (TOTAL_AREA_HA / 100) # arbitrary set to 1200 based on Kadzviti demo data 240HH x 5 (HH size)
farming_pop <- total_pop * 0.70 # Based on kadzviti focus group
HH_size <- 5 # average HH size based on Equity surveys
HH_number <- round(total_pop / HH_size, 0) # Number of households
HH_farming <- round(farming_pop / HH_size, 0) # Percentage of households farming (70% of total households) based on kadzviti demo
HH_wood_demand <- 1.7 # Household wood demand in tons per year 0.9 (tsotso stove) to 2.5, average = 1.7 ton/hh
farming_pop_rate <- 0.70


target_usd_per_capita_security <- 420 # Seuil objectif Food economic security, dignity = 250 USD, security = 420
target_usd_per_capita_dignity <- 250

RUE <- 2.35 # Rain Use Efficiency = 1.7 to 3 (Rufino 2011), 2.7 +-0.6 (Illius & O’Connor, 1999, Frost 1996)
HI_gdnuts <- 0.4 # Harvest index of groundnuts
NDFA_rate <- 0.62 # N derived from atmosphere rate from groundnuts

# Paramètres basés sur Frost 1996 pour calcul de wood availability
woody_coefficient <- 0.935
regeneration_years <- 28.5 # Years for full regeneration of miombo woodland calculated from Frost 1996, Gotore 2023 says 20 to 30 years


# TOBACCO CURING WOOD PARAMETERS
wood_demand_per_ha_tobacco <- 18.5 # current 5 to 22 tons /ha of tobacco


# Paramètres animaux
cattle_daily_intake <- 0.03 # fraction poids vif/jour
cattle_weight <- 250 # kg
winter_months <- 6 # , months june to october
wet_months <- 12 - winter_months # nov to may
cattle_security_threshold <- 4.5 # cattle/HH_farming # SEUIL DE SÉCURITÉ CATTLE 4 à 5 cattle / FHH
# Cattle: 250USD/head every 2 years, only when above threshold
usd_per_cattle_sale <- 250 / 2 # = 125 USD/cattle/year (annualized)


### Paramètres agro_pastoral loop
tolerance = 0.5
max_iterations = 30

#clay_waterlogging_penalty = 0.5  # watterlogging penalty for clay soils (80% reduction in yield potential)
# damage on crops from pest and diseases
damage_level = 0.7 # 30 % damage on crops from pest and diseases leaving 70% of the yield potential


# Paramètres fumier
manure_ton_per_TLU_per_year = 1.022 # 2.8 kg/day * 365 days / 1000 for tones Falconnier et al., 2023
collection_rate = 0.4 # Collectable manure rate Rusinamhodzi 2015 (40% of total manure produced, the rest is back to the environment)
N_content_manure = 0.014 # N content in manure (kg/kg) Falconnier et al., 2023
P_content_manure = 0.002 # P content in manure (kg/kg) Falconnier et al., 2023

# Paramètres résidus
residue_to_grain_ratio <- 1.5 # grain yields * 1.5 as shown in (Rusinamhodzi et al., 2015),
residue_degradation <- 0.75 # residue degradation rate (75% of residues are available for feeding cattle)
residue_feed_share <- 1 # ratio used to feed cattle (all is used for cattle feeding according to current practices)according to Madamombe et al., 2025)


# paramètres food security
min_starch_need <- 160 # kg/capita/year 160 to 200 kg/capita


# nutrition diversity
min_diversity_target <- 5 # Minimum target for nutrition diversity score
threshold_woodland_ha_per_capita <- 0.2 # 0.13 ha/capita minimum woodland area (current val)
#try up to 0.5

# NTFPs: valeur temporaire (mushrooms, honey, etc.)
usd_per_ha_ntfp <- 40 # Valeur temporaire à ajuster
