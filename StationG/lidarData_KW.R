library('neonUtilities')
library('neonOS')
dpID <- "DP1.30003.001"
siteID <- "MLBS"
setwd("StationG")
byTileAOP(dpID=dpID,
          site=siteID,
          year="2021",
          easting=542067,
          northing=4136943,
          check.size = FALSE) 