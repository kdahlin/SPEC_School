


# ------------------------------------------------------------------
# MLBS Ecophys Group: Slope-Specific Red Maple LAI Analysis
# Allometrics adapted from Brantley et al. (2016)
# ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra) # For arranging the plots side-by-side (if needed)

# ------------------------------------------------------------------
#Dataset from the Field 
# ------------------------------------------------------------------

field_data <- data.frame(
  tree_id  = 1:6,
  height_m = c(17.5, 11.5, 10.0, 12.0, 18.0, 12.0),
  dbh_cm   = c(29.0, 18.8, 32.5, 28.5, 25.1, 15.3),
  slope    = c("South", "North", "North", "South", "South", "North"),
  leaf_water_content = c(57.61, 63.67, 63.91, 60.82, 59.51, 58.17),
  mean_chlorophyll_content = c(27.67, 35.60, 32.30, 31.67, 37.03, 34.27),
  sla = c(129.7629607, 195.8964364, 159.2635226, 126.6231944, 121.8273906, 140.4191256)
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

# Slope-level ecological summaries
slope_summary <- ecophys_results %>% 
  group_by(slope) %>% 
  summarize(
    sample_size = n(),
    mean_leaf_area_m2 = mean(leaf_area_m2),
    mean_la_height_ratio = mean(la_to_height_ratio),
    mean_individual_lai = mean(individual_tree_lai),
    mean_water_content = mean(leaf_water_content),
    mean_chlorophyll = mean(mean_chlorophyll_content),
    mean_sla = mean(sla), # Added SLA summary metric
    .groups = "drop"
  )

# ------------------------------------------------------------------
# Summary 
# ------------------------------------------------------------------
print("--- Individual Tree Results ---")
print(ecophys_results)

print("--- Slope Summary ---")
print(slope_summary)

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




################################################################################

#                            Graphing

#################################################################################


library(dplyr)
library(ggplot2)
library(gridExtra) # For arranging the plots side-by-side



# ------------------------------------------------------------------ 
#                 Data Visualization (Boxplots)
# ------------------------------------------------------------------



library(tidyr) 

plot_data_long <- ecophys_results %>%
  select(slope, height_m, dbh_cm, leaf_water_content, 
         mean_chlorophyll_content, leaf_area_m2, 
         la_to_height_ratio, individual_tree_lai, sla) %>% # Added sla here
  # Reshape data from wide to long format
  pivot_longer(
    cols = -slope, 
    names_to = "variable", 
    values_to = "value"
  )

all_boxplots <- ggplot(plot_data_long, aes(x = slope, y = value, fill = slope)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, aes(color = slope)) +
  # Scales = "free_y" gives each variable its own unique Y-axis range
  facet_wrap(~ variable, scales = "free_y", ncol = 3) + 
  labs(
    title = "Ecophysiological Metrics by Slope Aspect",
    x = "Slope Aspect",
    y = "Measured Value"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10), 
    panel.spacing = unit(1, "lines")
  )

# Render the grid
print(all_boxplots)


# Ecological interpretation 

# South-Facing Samples exhibit higher values for 
# 1- diameter (DBH)
# 2- allometric equation derived total leaf area
# This suggest adaptation to an environment with higher light availability
# This could translate in greater vertical growth and expansive canopy development.

# North-Facing Samples display smaller median heights and total leaf areas, alongside wide variations in DBH

# Physiology considerations:

# Leaf Water Content (North > South): The north-facing trees display 
# higher median leaf water content (~63.7% vs. ~59.5%). 
# North-facing slopes are typically cooler, more humid, and less 
# prone to intense solar radiation, reducing evapotranspiration
# South-facing slopes experience higher solar radiation, 
# driving down foliar water retention.

# Chlorophyll Content (North > South): The median chlorophyll 
# content was slighly higher in the north-facing leaves. 
# This could be a physiological response to shade. 
# Under lower-light conditions (north slopes), plants often 
# upregulate chlorophyll concentration per unit leaf area to
# maximize light-harvesting efficiency in the shade.



# OTHER WAY (All different grapgs at once)

library(tidyr)
library(ggplot2)
library(dplyr)

variables_list <- unique(plot_data_long$variable)

for (var_name in variables_list) {
  
  # Filter data for just this variable
  single_var_data <- plot_data_long %>% 
    filter(variable == var_name)
  
  # Create the individual plot
  individual_plot <- ggplot(single_var_data, aes(x = slope, y = value, fill = slope)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, size = 2, aes(color = slope)) +
    labs(
      title = paste("Distribution of", var_name, "by Slope Aspect"),
      x = "Slope Aspect",
      y = var_name
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # Print to plot viewer
  print(individual_plot)
}



# Comparison of Neon and on-ground measurements 

library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Load the data
df <- data.frame(
  spad_cn = c(27.67, 35.60, 32.30, 31.67, 37.03, 34.27),
  neon_cn = c(2.11975, 2.06600, 2.49820, 2.16675, 2.42875, 2.52345),
  hemi_lai = c(3.84, 3.07, 4.46, 3.28, 3.98, 3.62),
  neon_lai = c(2.426636, 2.955943, 3.142970, 1.961802, 1.441730, 0.962188),
  hemi_file = c("101-1424", "101-1426", "101-1425", "101-1422", "101-1423", "101-1427"),
  note = c("ecophys group south", "ecophys group north", "ecophys group north", 
           "ecophys group south", "ecophys group south", "ecophys group north")
)

# Clean up the group labels for nicer plot aesthetics
df <- df %>%
  mutate(aspect = ifelse(grepl("north", note), "North", "South"))


# Convert to long format for clean faceting
df_long <- df %>%
  select(aspect, spad_cn, neon_cn, hemi_lai, neon_lai) %>%
  pivot_longer(cols = -aspect, names_to = "metric", values_to = "value")

# Generate comparative boxplots
ggplot(df_long, aes(x = aspect, y = value, fill = aspect)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 3, aes(color = aspect)) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  scale_color_manual(values = c("North" = "#C77CFF", "South" = "#7CAE00")) +
  labs(
    title = "Comparison of NEON and On-ground Measurements of LAI and Nitrogen by Slope Aspect", 
    x = "Slope Aspect", 
    y = "Measured Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# LAI Validation:NEON model vs Ground Reference

# Calculate the ideal limits to keep a perfectly square 1:1 ratio
max_val <- max(c(df$hemi_lai, df$neon_lai))
min_val <- min(c(df$hemi_lai, df$neon_lai))

ggplot(df, aes(x = hemi_lai, y = neon_lai)) +
  # Add the true 1:1 identity reference line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  # Linear model fit line
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, alpha = 0.15) +
  # Observed data points colored by aspect
  geom_point(aes(color = aspect), size = 4) +
  scale_color_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  # Force axes to be identical lengths so the 1:1 line sits at exactly 45 degrees
  coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
  labs(
    title = "LAI Validation: NEON Model vs. Ground Reference",
    subtitle = "Dashed line shows expected 1:1 ideal agreement",
    x = "Hemispherical Photo LAI (Ground Reference)",
    y = "NEON LAI (Remote Sensing Model)",
    color = "Aspect"
  ) +
  theme_bw(base_size = 14)



# LAI Validation:NEON model vs Ground Reference


library(ggplot2)
library(dplyr)

# Load the data
df <- data.frame(
  spad_cn = c(27.67, 35.60, 32.30, 31.67, 37.03, 34.27),
  neon_cn = c(2.11975, 2.06600, 2.49820, 2.16675, 2.42875, 2.52345),
  note = c("ecophys group south", "ecophys group north", "ecophys group north", 
           "ecophys group south", "ecophys group south", "ecophys group north")
)

# Clean group labels and calculate standard Z-scores
df <- df %>%
  mutate(
    aspect = ifelse(grepl("north", note), "North", "South"),
    spad_z = as.numeric(scale(spad_cn)),
    neon_z = as.numeric(scale(neon_cn))
  )


# Find global symmetric limits for a clean square plot
max_limit <- max(abs(c(df$spad_z, df$neon_z))) + 0.5

ggplot(df, aes(x = spad_z, y = neon_z)) +
  # 1:1 identity line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred", size = 1) +
  # Trendline with confidence interval
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, alpha = 0.15) +
  # Data points
  geom_point(aes(color = aspect), size = 4) +
  scale_color_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  # Force perfectly square layout
  coord_equal(xlim = c(-max_limit, max_limit), ylim = c(-max_limit, max_limit)) +
  labs(
    title = "Nitrogen Validation: NEON N% vs. SPAD",
    subtitle = "Standardized Z-Scores (Dashed line shows 1:1 ideal agreement)",
    x = "SPAD Chlorophyll Index (Standardized Reference)",
    y = "NEON N% Foliar Chemistry (Standardized Model)",
    color = "Aspect"
  ) +
  theme_bw(base_size = 14)



#   LAI Validation:NEON model vs Ground Reference with lines 


library(ggplot2)
library(dplyr)

# 1. Load Data
df <- data.frame(
  spad_cn = c(27.67, 35.60, 32.30, 31.67, 37.03, 34.27),
  neon_cn = c(2.11975, 2.06600, 2.49820, 2.16675, 2.42875, 2.52345),
  note = c("ecophys group south", "ecophys group north", "ecophys group north", 
           "ecophys group south", "ecophys group south", "ecophys group north")
)

# 2. Add aspect column and calculate standardized Z-scores
df <- df %>%
  mutate(
    aspect = ifelse(grepl("north", note), "North", "South"),
    spad_z = as.numeric(scale(spad_cn)),
    neon_z = as.numeric(scale(neon_cn))
  )

# Calculate global symmetric plot limits
max_limit <- max(abs(c(df$spad_z, df$neon_z))) + 0.5

# 3. Generate Graph
ggplot(df, aes(x = spad_z, y = neon_z)) +
  # Reference 1:1 line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred", size = 1) +
  
  # LINE 1: Global trendline (All Trees) - Black solid line
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, alpha = 0.1) +
  
  # LINES 2 & 3: Group-specific trendlines (North vs South) - Colored lines
  geom_smooth(aes(color = aspect, fill = aspect), method = "lm", formula = y ~ x, se = FALSE, size = 1.2) +
  
  # Observed Data points
  geom_point(aes(color = aspect), size = 4) +
  
  # Styling adjustments
  scale_color_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  scale_fill_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  coord_equal(xlim = c(-max_limit, max_limit), ylim = c(-max_limit, max_limit)) +
  labs(
    title = "Nitrogen Validation: NEON N% vs. SPAD",
    subtitle = "Standardized Z-Scores (Displaying Global and Aspect-Specific Trends)",
    x = "SPAD Chlorophyll Index (Standardized Reference)",
    y = "NEON N% Foliar Chemistry (Standardized Model)",
    color = "Slope Aspect",
    fill = "Slope Aspect"
  ) +
  theme_bw(base_size = 14)

labs(
  title = "Nitrogen Validation: NEON N% vs. SPAD",
  subtitle = paste("Global R² =", round(0.1223, 2), "| p =", round(0.4968, 2))
)

# Linear regresion 

# 1. Summary for All Trees
summary(lm(neon_z ~ spad_z, data = df))

# 2. Summary for North Slope Trees Only
summary(lm(neon_z ~ spad_z, data = df %>% filter(aspect == "North")))

# 3. Summary for South Slope Trees Only
summary(lm(neon_z ~ spad_z, data = df %>% filter(aspect == "South")))




# Adding the R squares 

library(ggplot2)
library(dplyr)

# 1. Load Data
df <- data.frame(
  spad_cn = c(27.67, 35.60, 32.30, 31.67, 37.03, 34.27),
  neon_cn = c(2.11975, 2.06600, 2.49820, 2.16675, 2.42875, 2.52345),
  note = c("ecophys group south", "ecophys group north", "ecophys group north", 
           "ecophys group south", "ecophys group south", "ecophys group north")
)

# 2. Add aspect column and calculate standardized Z-scores
df <- df %>%
  mutate(
    aspect = ifelse(grepl("north", note), "North", "South"),
    spad_z = as.numeric(scale(spad_cn)),
    neon_z = as.numeric(scale(neon_cn))
  )

# Calculate global symmetric plot limits
max_limit <- max(abs(c(df$spad_z, df$neon_z))) + 0.5

# 3. Generate Graph with correctly linked layers
ggplot(df, aes(x = spad_z, y = neon_z)) +
  # Reference 1:1 line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred", size = 1) +
  
  # LINE 1: Global trendline (All Trees) - Black solid line
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, alpha = 0.1) +
  
  # LINES 2 & 3: Group-specific trendlines (North vs South) - Colored lines
  geom_smooth(aes(color = aspect, fill = aspect), method = "lm", formula = y ~ x, se = FALSE, size = 1.2) +
  
  # Observed Data points
  geom_point(aes(color = aspect), size = 4) +
  
  # Styling adjustments
  scale_color_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  scale_fill_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  coord_equal(xlim = c(-max_limit, max_limit), ylim = c(-max_limit, max_limit)) +
  
  # Linked Labels Layer (Using your custom metrics)
  labs(
    title = "Nitrogen Validation: NEON N% vs. SPAD",
    subtitle = paste("Global R² =", round(0.1223, 2), "| p =", round(0.4968, 2)),
    x = "SPAD Chlorophyll Index (Standardized Reference)",
    y = "NEON N% Foliar Chemistry (Standardized Model)",
    color = "Slope Aspect",
    fill = "Slope Aspect"
  ) +
  theme_bw(base_size = 14)









library(tidyr)

# Reshape raw data to long format
df_cn_long <- df %>%
  select(aspect, spad_cn, neon_cn) %>%
  pivot_longer(cols = -aspect, names_to = "metric", values_to = "value") %>%
  mutate(metric = ifelse(metric == "spad_cn", "SPAD Index (Field)", "NEON N% (Airborne)"))

# Generate boxplots
ggplot(df_cn_long, aes(x = aspect, y = value, fill = aspect)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 3, aes(color = aspect)) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  scale_color_manual(values = c("North" = "#C77CFF", "South" = "#7CAE00")) +
  labs(
    title = "Nitrogen & Chlorophyll Metrics by Slope Aspect",
    x = "Slope Aspect",
    y = "Absolute Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")







library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Load the dataset
df <- data.frame(
  hemi_lai = c(3.84, 3.07, 4.46, 3.28, 3.98, 3.62),
  neon_lai = c(2.426636, 2.955943, 3.142970, 1.961802, 1.441730, 0.962188),
  note = c("ecophys group south", "ecophys group north", "ecophys group north", 
           "ecophys group south", "ecophys group south", "ecophys group north")
)

# 2. Add the aspect group
df <- df %>%
  mutate(aspect = ifelse(grepl("north", note), "North", "South"))

# 3. Reshape and label metrics for the facet headers
df_lai_long <- df %>%
  select(aspect, hemi_lai, neon_lai) %>%
  pivot_longer(cols = -aspect, names_to = "metric", values_to = "value") %>%
  mutate(metric = ifelse(metric == "neon_lai", "NEON LAI (Airborne)", "Hemispherical Photo (Field)"))

# 4. Generate the graph matching your exact theme and point coloring style
ggplot(df_lai_long, aes(x = aspect, y = value, fill = aspect)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  # Explicitly color the raw points to match the provided image (North = Purple, South = Green)
  geom_jitter(width = 0.1, size = 4, aes(color = aspect)) +
  facet_wrap(~metric, scales = "free_y") +
  # Box fill colors (Pink and Light Blue)
  scale_fill_manual(values = c("North" = "#F8766D", "South" = "#00BFC4")) +
  # Jitter point colors (Purple and Olive Green)
  scale_color_manual(values = c("North" = "#C77CFF", "South" = "#7CAE00")) +
  labs(
    title = "Leaf Area Index (LAI) Metrics by Slope Aspect",
    x = "Slope Aspect",
    y = "Absolute Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16),
    strip.text = element_text(size = 13),
    panel.grid.minor = element_blank()
  )















