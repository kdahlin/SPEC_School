library(terra)
library(neonUtilities)
library(neonOS)
library(devtools)
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)

veglist <- loadByProduct(dpID="DP1.10098.001", 
                         site="MLBS", 
                         package="basic", 
                         release="RELEASE-2023",
                         check.size = FALSE)

vegmap <- getLocTOS(veglist$vst_mappingandtagging, 
                    "vst_mappingandtagging")

veg <- joinTableNEON(veglist$vst_apparentindividual, 
                     vegmap, 
                     name1="vst_apparentindividual",
                     name2="vst_mappingandtagging")

symbols(veg$adjEasting[which(veg$plotID=="MLBS_002")], 
        veg$adjNorthing[which(veg$plotID=="MLBS_002")], 
        circles=veg$stemDiameter[which(veg$plotID=="MLBS_002")]/100/2, 
        inches=F, xlab="Easting", ylab="Northing")
