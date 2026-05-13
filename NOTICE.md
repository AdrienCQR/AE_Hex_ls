# AE Hex Landscape — User Guide

Agroecological indicator analysis at the hexagonal landscape scale for Murehwa District, Zimbabwe.  
This folder is **self-contained**: all R scripts needed to run the full pipeline are included.

---

## Quick start

Open the project in RStudio, then run:

```r
source("AE_Hex_ls/RUN_MAIN.R")
```

The central script `RUN_MAIN.R` runs all 4 steps in order.  
Steps 1 and 2 have **checkpoints**: if the output file already exists, the step is skipped.  
This lets you restart from step 3 (maps) without recomputing the indicators.

---

## The 4 pipeline steps

| Step | Script | Description |
|------|--------|-------------|
| 0 | `script/00_config.R` | Parameters and file paths |
| 1 | `script/01_build_hex_database.R` | Build the hexagonal landscape database |
| 2 | `script/02_run_indicators.R` | Calculate all agroecological indicators |
| 3 | `script/03_maps.R` | Produce choropleth maps |
| 4 | `script/04_synergies.R` | Synergy / loss / trade-off analysis |

---

## Step descriptions

### Step 0 — Parameters (`00_config.R`)

**The only file you need to edit** to adapt the analysis:
- File paths (`FILE_PATHS`)
- Which indicators to run (`INDICATORS_TO_RUN`)
- Biophysical parameters (rainfall, land areas, crop shares, livestock, etc.)

### Step 1 — Build hexagonal database (`01_build_hex_database.R`)

Creates the 5 km² hexagonal grid over Murehwa and extracts the LULC composition of each cell.

**Inputs:**
- `data/raw/lulc/V2_land_use_map_garden_32736.tif`
- `data/raw/communal_wards/communal_wards.shp`

**Outputs:**
- `AE_Hex_ls/data/lulc_processed/V2_lulc_reclassified.tif`
- `AE_Hex_ls/data/grid_tool/V4_grid_hex_5km2_filtered_final.shp`
- `AE_Hex_ls/data/lulc_processed/lulc_compo_mur_ward_towm_filtered_v4.csv`

> **Checkpoint:** skipped on the next run if the CSV already exists.

Filtering criteria applied to hexagons:
- Full LULC coverage (no NoData pixels)
- Intersects at least one communal ward
- Excludes Murehwa town hexagons (IDs 441, 459, 460, 477)
- Cropland proportion ≥ 10%
- Mineral / bare soil proportion ≤ 30%

### Step 2 — Calculate indicators (`02_run_indicators.R`)

Loads landscape compositions, runs all active indicators, and builds an AE composite score (normalised geometric mean).

**Inputs:**
- `lulc_compo_mur_ward_towm_filtered_v4.csv`
- `data/processed/baseline_compo.csv`
- `AE_Hex_ls/indicators/run_pipeline.R` (indicator engine)

**Output:**
- `AE_Hex_ls/results/v3_final_results_with_composite_scores.csv`

> **Checkpoint:** skipped if the results file already exists.

Indicators computed (toggle each in `00_config.R`):

| Column | Indicator |
|--------|-----------|
| `final_N_total_per_ha` | Nutrient recycling (N per maize ha) |
| `final_P_total_per_ha` | Nutrient recycling (P per maize ha) |
| `soc_density_ha` | Soil organic carbon density |
| `biodiv_score` | Biodiversity habitat integrity |
| `total_N_legumes` | N input from legume rotation |
| `wood_availability_score` | Firewood sustainability |
| `max_grazing_CC_TLU` | Grazing carrying capacity |
| `final_maize_yield_avg` | Maize yield (agro-pastoral loop) |
| `economic_diversification_score` | Economic diversification (Shannon) |
| `equity_score` | Equity (reversed Gini) |
| `connectivity_score_brut` | Value chain connectivity |
| `synergy_score_sum` | Synergy score |
| `ae_composite_score` | AE composite (geometric mean) |

### Step 3 — Maps (`03_maps.R`)

Produces one choropleth map per indicator with OpenStreetMap basemap, Jenks classification, and ward name labels.

**Outputs:** PNG files in `AE_Hex_ls/results/maps3/`

### Step 4 — Synergy / loss / trade-off analysis (`04_synergies.R`)

Implements the pairwise synergy/loss/trade-off framework (Leroux et al., 2022):
- Classifies each indicator into 4 quantile classes
- Identifies synergies (both high), losses (both low), and trade-offs (one high / one low) for each pair and each hexagon
- Classifies relationship intensity (none / weak / moderate / strong)
- Determines the dominant relationship per hexagon
- Produces pairwise heatmaps (percentage of hexagons per relationship type)

**Outputs:**
- `map_synergy_losses_tradeoffs.png`
- `heatmap_synergies_tradeoffs_losses.png`

---

## Folder structure

```
AE_Hex_ls/
├── RUN_MAIN.R                          <- entry point
├── NOTICE.md                           <- this file
├── script/
│   ├── 00_config.R                     <- parameters
│   ├── 01_build_hex_database.R         <- step 1
│   ├── 02_run_indicators.R             <- step 2
│   ├── 03_maps.R                       <- step 3
│   └── 04_synergies.R                  <- step 4
├── indicators/
│   ├── run_pipeline.R                  <- indicator engine
│   ├── tree_hedges_processing.R
│   ├── SOIL_HEALTH.R
│   ├── BIODIV.R
│   ├── N_LITTER_TREES.R
│   ├── N_LEGUMES_ROTATION.R
│   ├── GRAZING_CC.R
│   ├── AGRO_PASTO_LOOP.R
│   ├── ECO_DIV.R
│   ├── FAIRNESS.R
│   ├── WOOD.R
│   ├── CONNECTIVITY_VAL_CHAIN.R
│   └── SYNERGY.R
├── data/
│   ├── grid_tool/                      <- hexagonal grid shapefiles (step 1 output)
│   ├── lulc_processed/                 <- rasters and LULC composition CSVs (step 1 output)
│   ├── raw/                            <- source data to copy here (not tracked by Git)
│   │   ├── lulc/                       <- V2_land_use_map_garden_32736.tif + codes CSV
│   │   ├── communal_wards/             <- communal_wards.shp
│   │   └── data_indicators/            <- RESULTS_mean_pref_LU_groups.csv (FAIRNESS)
│   └── processed/                      <- baseline_compo.csv, AGB_median_2010.csv
└── results/
    ├── v3_final_results_with_composite_scores.csv
    └── maps/                           <- maps and figures
```

**Source data to copy into `AE_Hex_ls/data/`** (not tracked by Git):

| File | Destination |
|------|-------------|
| `V2_land_use_map_garden_32736.tif` | `AE_Hex_ls/data/raw/lulc/` |
| `V2_land_use_map_codes.csv` | `AE_Hex_ls/data/raw/lulc/` |
| `communal_wards.shp` (+ .dbf/.shx/.prj) | `AE_Hex_ls/data/raw/communal_wards/` |
| `RESULTS_mean_pref_LU_groups.csv` | `AE_Hex_ls/data/raw/data_indicators/` |
| `baseline_compo.csv` | `AE_Hex_ls/data/processed/` |
| `AGB_median_2010.csv` | `AE_Hex_ls/data/processed/` |

> If you already have `lulc_compo_mur_ward_towm_filtered_v4.csv` (step 1 output) and
> `v3_final_results_with_composite_scores.csv` (step 2 output), you can skip to step 3
> without needing any raw data files.

---

## Running a single step

To run one step independently:

```r
source("AE_Hex_ls/script/00_config.R")          # always load first

source("AE_Hex_ls/script/01_build_hex_database.R")
source("AE_Hex_ls/script/02_run_indicators.R")
source("AE_Hex_ls/script/03_maps.R")
source("AE_Hex_ls/script/04_synergies.R")
```

---

## Required R packages

```r
install.packages(c(
  "sf", "terra", "dplyr", "tidyr", "ggplot2", "readr",
  "classInt", "ggspatial", "paletteer", "ggrepel",
  "exactextractr", "units", "patchwork", "cowplot",
  "reshape2", "viridis", "RColorBrewer"
))
```
