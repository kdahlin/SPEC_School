library('raster')
library('rgdal')
library('sp')
library('tidyverse')
#Based on tutorial at: https://www.neonscience.org/resources/learning-hub/tutorials/dc-open-shapefiles-r

#This assumes you have downloaded the shape files from the HPCC: /mnt/home/f0107143/labFolder/shared_data/NEON_AOP_data/all_GIS/90percentFlux
footPrintFileName <- "footprintData/90percent_flux.shp"

#Read in shape file
footprintShp <- readOGR(footPrintFileName)

#Look at data
class(footprintShp)
crs(footprintShp)
extent(footprintShp)
footprintShp #View all metadata at the same time

#Subset to just MLBS and the major footprint
mlbs_footprint=footprintShp[footprintShp$SiteID=="MLBS" & footprintShp$Type=="major",]

#Plot footpring
plot(mlbs_footprint)


