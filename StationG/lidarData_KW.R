library('neonUtilities')
library('neonOS')
library('lidR')
#devtools::install_github("akamoske/canopyLazR")
library('canopyLazR')
dpID <- "DP1.30003.001"
siteID <- "MLBS"
setwd("StationG")
byTileAOP(dpID=dpID,
          site=siteID,
          year="2021",
          easting=542067,
          northing=4136943,
          check.size = FALSE) 

lidarFileName <- "DP1.30003.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L1/DiscreteLidar/ClassifiedPointCloud/NEON_D07_MLBS_DP1_542000_4136000_classified_point_cloud_colorized.laz"
lidarDat <- readLAS(lidarFileName)
plot(lidarDat) #Three "levels" appear and the top and bottom are noise

lidarDat_filtered <- filter_poi(lidarDat,Z>1100, Z<1250) #Z is m above sea level
  
plot(lidarDat_filtered) #Not looking as clean as if you clipped it on cloudcompare

