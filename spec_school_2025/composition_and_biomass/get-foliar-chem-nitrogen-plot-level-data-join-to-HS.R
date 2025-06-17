##################
# Load NEON plots
##################
plots <- st_read("lidar/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Centroids_V11.shp")

# plots of interest
# MLBS_061 to 075 and 002 and 009

library(neonUtilities)

# Set your working directory again if needed
wd <- "C:\\Users\\mille\\Documents\\NAU 2024-2026\\spec-school-summer-2025\\"
setwd(wd)

# Download foliar chemistry data (DP1.10026.001)
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

# Now join just once per plot
fc_joined <- fc_filtered_unique %>%
  left_join(plots_unique, by = "plotID") %>%
  st_as_sf() %>%
  st_transform(crs = h5CRS)

fc_vect <- vect(fc_joined)

plot_spectra <- terra::extract(hsStack, fc_vect, fun = mean, na.rm = TRUE)

spectra_with_traits <- cbind(fc_filtered_unique, plot_spectra[,-1])

spectra_with_traits <- spectra_with_traits %>%
  select(where(~ !all(is.na(.))))

glimpse(spectra_with_traits)

plot(hsStack[[1]], main = "Plot locations over Band 1 of Hyperspectral Image")
plot(fc_vect, add = TRUE, col = "red", pch = 16)

as.data.frame(geom(fc_vect))

### TO DO TOMORROW - RE DO AOP DOWNLOAD,
### FIGURE OUT HOW TO GET THE TILES THAT CORRESPOND TO THE PLOTS