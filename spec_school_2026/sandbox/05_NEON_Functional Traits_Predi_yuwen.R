library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)


# Step1 Load and Combine files
field_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/NEONForestAGBv2_Jenkins_MLBS_plot_2022.csv')
view(field_df)

spc_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/MLBS_VIs_PCA_Hyperspectral.csv')
dim(spc_df)
head(spc_df)

spec_agb <-
lidar_df

spectra_long_agb <- spectra_long %>%
  left_join(agb_plot, by = "plotID")

# Step2