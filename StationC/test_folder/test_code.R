library('raster')

setwd('/Users/isamarcortes/Documents/SPEC_School/StationC')###change this to your local repo

KamoskeData <- read.csv('Kamoske_etal_2022_data/all_metrics_20200803.csv')###csv file
agb <- raster('MLBS_agbEcoregion_20m.tif')###agb raster
plot(agb)

shapefile <- st_read('Kamoske_etal_2022_data/neon_plots_mlbs.shp')###shapefile data
plot(shapefile,add=T)

shapefileAGB <- as.data.frame(extract(agb,shapefile,fun=mean))###taking average for each shapefile
shapefileAGB$plot <- shapefile$id ###adds ID to shapefileAGB
colnames(shapefileAGB)<- c("Biomass Raster","plotID")
newDF <- merge(KamoskeData,shapefileAGB,by='plotID')

plot()
