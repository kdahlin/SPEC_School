## ----set-wd, results="hide"--------------------------------------------------------------------------------------------------------------------------------------------------
wd <- "C:\\Users\\mille\\Documents\\NAU 2024-2026\\spec-school-summer-2025\\" #This will depend on your local environment
setwd(wd)

library(dplyr)
library(terra)
library(tidyverse)
library(ggplot2)
library(sf)

# Load NEON plots
plots <- st_read("lidar/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Centroids_V11.shp")

# plots of interest
# MLBS_061 to 075 and 002 and 009
library(rhdf5)
library(neonUtilities)

# Download foliar chemistry data
fc <- loadByProduct(dpID = "DP1.10026.001",
                    site = "MLBS",
                    startdate = "2023-01",
                    enddate = "2023-12",
                    package = "expanded",
                    check.size = TRUE)

fc_cfc_nitrogen <- fc$cfc_carbonNitrogen

glimpse(fc_cfc_nitrogen)

fc_filtered <- fc_cfc_nitrogen %>%
  filter(plotID %in% paste0("MLBS_", sprintf("%03d", c(2, 9, 61:75)))) %>%
  select(plotID, sampleID, collectDate, nitrogenPercent) %>%
  filter(!is.na(nitrogenPercent))

# Check available columns in shapefile
names(plots)

fc_filtered_unique <- fc_filtered %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

# Ensure the shapefile is unique by plotID
plots_unique <- plots %>%
  filter(plotID %in% fc_filtered_unique$plotID) %>%
  distinct(plotID, .keep_all = TRUE)
