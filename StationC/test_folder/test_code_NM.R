setwd("F:/Postdoc/SPECSchool/SPEC_School/StationC/data")
library(terra)
library(dplyr)
library(ggplot2)

# Read Raster of AGB
biom<-rast("MLBS_agbEcoregion_20m.tif")
plot(biom)

#Read csv file with data about all neon plots with metrics
plots<-read.csv("Kamoske_etal_2022_data/Kamoske_etal_2022_data/all_metrics_20200803.csv")
plotm<-plots%>%filter(siteID=="MLBS") # MLBS plots only

# Read shapefile with mlbs plot locations
div<-vect("Kamoske_etal_2022_data/Kamoske_etal_2022_data/neon_plots_mlbs.shp")
plot(div)

plot_intersect<-extract(biom, div, mean, na.rm=T) # extracting values of biomass at MLBS neon plots
plot_intersect<-cbind(plotID=div$id,plot_intersect) # combining to get plot IDs

plots_combined<-merge(plot_intersect, plotm, by="plotID")

colnames(plots_combined)[3]<-"AGB" # Renaming AG biomass column 

ggplot(plots_combined, aes(diversity_shannon, AGB))+geom_point() #Plotting


summary(lm(AGB~ dsm_mean, data=plots_combined))
