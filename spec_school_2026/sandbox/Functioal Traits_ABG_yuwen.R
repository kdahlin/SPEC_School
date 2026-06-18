# install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

agb_df <- read.csv("C:/Users/tzpan/Downloads/NEONForestAGBv2.csv")
names(agb_df)
agb_df1 <- agb_df %>% filter(siteID == 'MLBS')
dim(agb_df1)

write.csv(agb_df1,'C:/Users/tzpan/Downloads/NEONForestAGBv2_MLBS.csv')


agb_df <- read.csv('C:/Users/tzpan/Downloads/NEONForestAGBv2_MLBS.csv')
dim(agb_df)
names(agb_df)
levels(as.factor(agb_df$allometry))

agb_df1 <- agb_df %>% filter(allometry == 'AGBJenkins')
dim(agb_df1)
names(agb_df1)
str(agb_df1)
agb_df1$date <- as.Date(agb_df1$date)

range(year(as.Date(agb_df$date))) #2015 2022

agb_df2 <- agb_df1 %>%
  filter(year(date) == 2022)
dim(agb_df2)
write.csv(agb_df2,'C:/Users/tzpan/Downloads/NEONForestAGBv2_Jenkins_MLBS_2022.csv')

agb_df2 <- read.csv('C:/Users/tzpan/Downloads/NEONForestAGBv2_Jenkins_MLBS_2022.csv')
unique(agb_df2$plotID)
# next step aggregate tree-level to plot-level
# first sum up the ABG in same plotID, then divided the areas
levels(as.factor(agb_df2$plotID))
head(agb_df2)

plot_area <- pi * 20^2   # 1256.637 m²

agb_plot <- agb_df2 %>%
  group_by(plotID) %>%
  summarise(
    total_AGB_kg = sum(AGB, na.rm = TRUE),
    n_trees = n(),
    .groups = "drop"
  )

agb_plot <- agb_plot %>%
  mutate(
    AGB_kg_m2 = total_AGB_kg / plot_area,
    AGB_Mg_ha = AGB_kg_m2 * 10
  )

head(agb_plot)
write.csv(agb_plot,'C:/Users/tzpan/Downloads/NEONForestAGBv2_Jenkins_MLBS_plot_2022.csv')

# Spectra
spectra_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/MLBS_plot_spectra.csv')
dim(spectra_df) #36plots*375bands
head(spectra_df)

spectra_long <- spectra_df %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "band",
    values_to = "reflectance"
  )

spectra_long <- spectra_long %>%
  mutate(
    wavelength = as.numeric(gsub("X", "", band))
  )

head(spectra_long)

spectra_long %>% ggplot(aes(x = wavelength, y = reflectance/ 10000, color = plotID, group = plotID)) +
  geom_line(alpha = 0.3) +
  theme_minimal() +
  labs(x = "Wavelength (nm)", y = "Reflectance")

# A. For modeling
spectra_agb <- spectra_df %>%
  left_join(agb_plot, by = "plotID")
head(spectra_agb)

# B. For plotting spectra with biomass context
spectra_long_agb <- spectra_long %>%
  left_join(agb_plot, by = "plotID")


# How ABG gradiance related to Spectra
# spectral sensitivity vs biomass
ggplot(spectra_long_agb,
       aes(x = wavelength,
           y = reflectance / 10000,
           group = plotID,
           color = AGB_Mg_ha)) +
  geom_line() +
  scale_color_viridis_c() +
  theme_minimal()

summary(spectra_long_agb$AGB_Mg_ha)

# Correlation

band_cols <- grep("^X", names(spectra_agb), value = TRUE)


cor_df <- data.frame(
  band = band_cols,
  wavelength = as.numeric(gsub("X", "", band_cols)),
  cor = sapply(band_cols, function(b) {
    cor(spectra_agb[[b]],
        spectra_agb$AGB_Mg_ha,
        use = "complete.obs")
  })
)

ggplot(cor_df, aes(x = wavelength, y = cor)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Wavelength (nm)",
       y = "Correlation with AGB")

# cor_df <- data.frame(
#   band = band_names,
#   cor = map_dbl(band_names, ~ cor(spectra_agb[[.x]],
#                                   spectra_agb$AGB_Mg_ha,
#                                   use = "complete.obs"))
# )