#install.packages(c("neonUtilities", "dplyr", "tidyr", "terra"))

library(neonUtilities)
library(dplyr)
library(ggplot2)
library(tidyr)

## Load vegetation structure data
# veg_data <- loadByProduct(dpID = "DP1.10098.001", site = "MLBS", 
#                           startdate = "2023-01", enddate = "2023-12", 
#                           package = "basic", check.size = FALSE)

# Extract tables
vst_ai  <- veg_data$vst_apparentindividual        # Tree measurements per stem
vst_map <- veg_data$vst_mappingandtagging         # Taxonomy and location
vst_ppy <- veg_data$vst_perplotperyear            # Plot metadata with lat/lon

# Filter vst_ai for 2023 only
vst_ai_2023 <- vst_ai %>%
  filter(format(date, "%Y") == "2023")

# Join on individualID to add taxonomy (genus/family)
vst_merged <- vst_ai_2023 %>%
  left_join(vst_map %>% select(individualID, taxonID, scientificName, genus, family), 
            by = "individualID")

# Remove any records without plotID or family
vst_merged <- vst_merged %>% 
  filter(!is.na(plotID), !is.na(family))

# Count Fabaceae per plot
fabaceae_plot_counts <- vst_merged %>%
  filter(family == "Fabaceae") %>%
  count(plotID, name = "fabaceae_count")

# Count total individuals per plot
total_plot_counts <- vst_merged %>%
  count(plotID, name = "total_count")

# Join and calculate relative Fabaceae abundance
fabaceae_summary <- total_plot_counts %>%
  left_join(fabaceae_plot_counts, by = "plotID") %>%
  mutate(fabaceae_count = ifelse(is.na(fabaceae_count), 0, fabaceae_count),
         fabaceae_rel_abundance = fabaceae_count / total_count)

# Merge lat/lon info for plotting or matching to raster
plot_locations <- vst_ppy %>%
  select(plotID, decimalLatitude, decimalLongitude, easting, northing, utmZone)

fabaceae_summary <- fabaceae_summary %>%
  left_join(plot_locations, by = "plotID")

# Preview result
head(fabaceae_summary)

fabaceae_summary %>%
  filter(fabaceae_count > 0) %>%
  summarize(plots_with_fabaceae = n())

vst_merged %>%
  filter(family == "Fabaceae") %>%
  group_by(plotID) %>%
  summarize(fabaceae_count = n(), .groups = "drop")

