


# ------------------------------------------------------------------
# MLBS Ecophys Group: Slope-Specific Red Maple LAI Analysis
# Allometrics adapted from Brantley et al. (2016)
# ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# dataset from the field
field_data <- data.frame(
  tree_id  = 1:6,
  height_m = c(17.5, 11.5, 10.0, 12.0, 18.0, 12.0),
  dbh_cm   = c(29.0, 18.8, 32.5, 28.5, 25.1, 15.3),
  slope    = c("South", "North", "North", "South", "South", "North")
)

# Brantley et al 2016 Table 6 Coefficients (Foliage Area m2 for ACRU)
# https://research.fs.usda.gov/download/treesearch/52752.pdf

a_slope     <- 1.2380  
b_intercept <- 0.1016  


# Individual Tree Metrics - Individual Leaf Area (LA)
ecophys_results <- field_data %>%
  mutate(
    # Calculate Total Leaf Area (LA) using allometrics
    log10_LA     = a_slope * log10(dbh_cm) + b_intercept,
    leaf_area_m2 = 10^(log10_LA),
    
    # Metric A: Leaf Area to Height Investment Ratio 
    # (m2 of leaves per meter of height) ** THIS IS A ROUGH ESTIMATION **
    la_to_height_ratio = leaf_area_m2 / height_m,
    
    # Metric B: Individual Tree Crown LAI Proxy
    # Approximating individual crown radius based on 
    # temperate hardwood DBH scaling:
    # Radius (m) ≈ 0.12 * DBH (cm). Crown Area = pi * r^2
    # * ANOTHER PRETTY ROUGH ESTIMATION **
    estimated_crown_area_m2 = pi * ((0.12 * dbh_cm)^2),
    individual_tree_lai     = leaf_area_m2 / estimated_crown_area_m2
  )

#  Slope-Level Ecological Summaries
slope_summary <- ecophys_results %>%
  group_by(slope) %>%
  summarize(
    sample_size          = n(),
    mean_leaf_area_m2    = mean(leaf_area_m2),
    mean_la_height_ratio = mean(la_to_height_ratio),
    mean_individual_lai  = mean(individual_tree_lai),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# Summary 
# ------------------------------------------------------------------
cat("=== INDIVIDUAL ECOPHYS METRICS ===\n")
print(ecophys_results %>% 
        select(tree_id, slope, dbh_cm, height_m, leaf_area_m2, la_to_height_ratio, individual_tree_lai) %>%
        mutate(across(where(is.numeric), ~ round(., 2))) %>%
        arrange(slope))

cat("\n=== SLOPE-LEVEL COMPARATIVE ADVANTAGES ===\n")
print(slope_summary %>% mutate(across(where(is.numeric), ~ round(., 3))))


# Potential outcome from this rough preliminary data:
# The Leaf Area:Height Ratio: 
# - Trees on the North slope generate more leaf area per vertical
# meter of growth (5.55 m²/m) compared to the South slope (5.04 m²/m). 
# This could suggest the expected shade-adaptation of the north slope trees, 
# efficient leaf layers to optimize light capture in a damp, competitive environment.

# Potential Individual LAI Conclusion: 
# This data suggest that the structure of the individual tree crowns is higher 
# on the North aspect (2.82) than the South aspect (2.24), individual red 
# maples on south-facing terrains seems to have more open, light-permeable foliage 
# architectures to cope with greater solar heat load in the peak of the summer.

