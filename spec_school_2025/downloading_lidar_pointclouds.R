# Script for downloading NEON tiles

# need the neonUtilities library
library(neonUtilities)

# saving these to my local machine then will move to HPCC with globus 
# see https://docs.icer.msu.edu/Transferring_data_with_Globus/
setwd("C:/geo_temp/MLBS_2022/")

# min / max easting and northing found by sorting on the NEON data portal
min.easting <- 535000
max.easting <- 547000
min.northing <- 4131000
max.northing <- 4143000

# make a list of the easting values by 1000s (units are meters, so 1 km = 1000 m)
easting.list <- seq(from = min.easting,
                    to = max.easting, 
                    by = 1000)


northing.list <- seq(from = min.northing,
                     to = max.northing,
                     by = 1000)

east.north.combos <- as.data.frame(matrix(NA, 
                                          nrow = (length(easting.list) * 
                                                       length(northing.list)),
                                   ncol = 2))

names(east.north.combos) <- c("easting", "northing")

east.north.combos$easting <- rep(easting.list, 
                                 length(northing.list))

east.north.combos$northing <- rep(northing.list, each = length(easting.list))

for (i in 3:nrow(east.north.combos)) {
  byTileAOP(dpID = "DP1.30003.001",
          site = "MLBS",
          year = 2022,
          easting = east.north.combos$easting[i],
          northing = east.north.combos$northing[i],
          buffer = 0,
          check.size = FALSE)
  print(paste("done with", i, "of", nrow(east.north.combos)))
}
