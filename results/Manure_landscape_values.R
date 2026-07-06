#Double check Manure values

library(dplyr)

INPUT_CSV_PATH <- "C:/WORK/CODE/AE_Hex_ls/results/v3_final_results_with_composite_scores.csv"

MANURE_T_PER_TLU <- 1.022   # t de fumier produit / UBT / an

df <- read.csv(INPUT_CSV_PATH, stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    manure_t_per_maize_ha = (final_TLU * MANURE_T_PER_TLU) / maize_ha
  )

boxplot(df$manure_t_per_maize_ha, main = "Boxplot of Manure Production per Maize Hectare", ylab = "Manure (t/ha)", col = "lightblue")
# print values:
print(summary(df$manure_t_per_maize_ha))


# apply 0.4 coef (represent collection rate of manure produced)
df <- df %>%
  mutate(
    manure_collected_t_per_maize_ha = manure_t_per_maize_ha * 0.4
  )

summary(df$manure_collected_t_per_maize_ha)
