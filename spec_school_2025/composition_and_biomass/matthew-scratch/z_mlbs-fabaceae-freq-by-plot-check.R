library(neonUtilities)
library(dplyr)
library(ggplot2)

# Step 1: Download 2021–2023 NEON vegetation data for MLBS
veg_data_3yrs <- loadByProduct(dpID = "DP1.10098.001", site = "MLBS",
                               startdate = "2021-01", enddate = "2023-12",
                               package = "basic", check.size = FALSE)

# Step 2: Extract relevant tables
vst_ai  <- veg_data_3yrs$vst_apparentindividual
vst_map <- veg_data_3yrs$vst_mappingandtagging

# Step 3: Add year and join taxonomy
vst_ai <- vst_ai %>%
  mutate(year = format(date, "%Y")) %>%
  left_join(vst_map %>% select(individualID, family), by = "individualID")

# Step 4: Filter for Fabaceae and count per plot per year
fabaceae_counts <- vst_ai %>%
  filter(family == "Fabaceae", !is.na(plotID)) %>%
  count(plotID, year, name = "fabaceae_count")

# Step 5: Create faceted bar plot
ggplot(fabaceae_counts, aes(x = plotID, y = fabaceae_count, fill = plotID)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Fabaceae Counts per Plot per Year (MLBS)",
    x = "Plot ID",
    y = "Number of Fabaceae Individuals"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
