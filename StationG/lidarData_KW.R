library('neonUtilities')
library('neonOS')
library('lidR')
devtools::install_github("akamoske/canopyLazR")
library('canopyLazR')
dpID <- "DP1.30003.001"
siteID <- "MLBS"
setwd("D:/Applications/SPEC_School/Lidar")
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
#save it
writeLAS(lidarDat_filtered, "lidarDat_filtered.laz")

#read it 

# Convert .laz or .las file into a voxelized lidar array
laz.data <- laz.to.array("lidarDat_filtered.laz", 
                         voxel.resolution = 10, 
                         z.resolution = 1,
                         use.classified.returns = TRUE)

# Level the voxelized array to mimic a canopy height model
level.canopy <- canopy.height.levelr(lidar.array = laz.data)

# Estimate LAD for each voxel in leveled array
lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                             voxel.height = 1, 
                             beer.lambert.constant = NULL)

# Convert the LAD array into a single raster stack
lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates, 
                                        laz.array = laz.data, 
                                        epsg.code = 32611)

# Create a single LAI raster from the LAD raster stack
lai.raster <- raster::calc(lad.raster, fun = sum, na.rm = TRUE)

# Convert the list of LAZ arrays into a ground and canopy height raster
grd.can.rasters <- array.to.ground.and.canopy.rasters(laz.data, 32611)

# Calculate max LAD and height of max LAD
max.lad <- lad.ht.max(lad.array = lad.estimates, 
                      laz.array = laz.data, 
                      ht.cut = 5, 
                      epsg.code = 32618)

# Calculate the ratio of filled and empty voxels in a given column of the canopy
empty.filled.ratio <- canopy.porosity.filled.ratio(lad.array = lad.estimates,
                                                   laz.array = laz.data,
                                                   ht.cut = 5,
                                                   epsg.code = 32618)

# Calculate the volume of filled and empty voxles in a given column of the canopy
empty.filled.volume <- canopy.porosity.filled.volume(lad.array = lad.estimates,
                                                     laz.array = laz.data,
                                                     ht.cut = 5,
                                                     xy.res = 10,
                                                     z.res = 1,
                                                     epsg.code = 32618)

# Calculate the within canopy rugosity
within.can.rugosity <- rugosity.within.canopy(lad.array = lad.estimates,
                                              laz.array = laz.data,
                                              ht.cut = 5,
                                              epsg.code = 32618)

# Calculate the heights of various LAD quantiles
ht.quantiles <- lad.quantiles(lad.array = lad.estimates,
                              laz.array = laz.data,
                              ht.cut = 5,
                              epsg.code = 32618)

# Calculate various canopy volume metrics from Lefsky
can.volume <- canopy.volume(lad.array = lad.estimates,
                            laz.array = laz.data,
                            ht.cut = 5,
                            xy.res = 10,
                            z.res = 1,
                            epsg.code = 32618)

# We can calculate the depth of the euphotic zone by dividing by the volume of the voxel
euphotic.depth <- can.volume$euphotic.volume.column.raster / ( 10 * 10 * 1)

# Calculate the top of canopy rugosity volume
toc.rugos <- toc.rugosity(chm.raster = grd.can.rasters$chm.raster,
                          xy.res = 10,
                          z.res = 1)

# Plot the lai raster
plot(lai.raster)

# Plot the ground raster
plot(grd.can.rasters$ground.raster)

# Plot the canopy height raster
plot(grd.can.rasters$canopy.raster)

# Plot the canopy height model raster
plot(grd.can.rasters$chm.raster)

# Plot the max LAD raster
plot(max.lad$max.lad.raster)

# Plot the height of max LAD raster
plot(max.lad$max.lad.ht.raster)

# Plot filled voxel ratio raster
plot(empty.filled.ratio$filled.raster)

# Plot porosity voxel ratio raster
plot(empty.filled.ratio$porosity.raster)

# Plot filled voxel volume raster
plot(empty.filled.volume$filled.raster)

# Plot porosity voxel volume raster
plot(empty.filled.volume$porosity.raster)

# Plot the standard deviation of LAD within a vertical column raster
plot(within.can.rugosity$vertical.sd.lad.raster)

# Plot within canopy rugosity
plot(within.can.rugosity$rugosity.raster)

# Plot the height of the 10th quantile
plot(ht.quantiles$quantile.10.raster)

# Plot the height of the 25th quantile
plot(ht.quantiles$quantile.25.raster)

# Plot the height of the 50th quantile
plot(ht.quantiles$quantile.50.raster)

# Plot the height of the 75th quantile
plot(ht.quantiles$quantile.75.raster)

# Plot the height of the 90th quantile
plot(ht.quantiles$quantile.90.raster)

# Plot the height of the mean LAD
plot(ht.quantiles$mean.raster)

# Plot the volume of the euphotic zone for each column
plot(can.volume$euphotic.volume.column.raster)

# Plot the total leaf area in the euphotic zone for each column
plot(can.volume$euphotic.tla.column.raster)

# Plot the depth of the euphotic zone
plot(euphotic.depth)

# Plot the volume of the oligophotic zone for each column
plot(can.volume$oligophotic.volume.column.raster)

# Plot the total leaf area in the oligophotic zone for each column
plot(can.volume$oligophotic.tla.column.raster)

# Plot the volume of the empty space within a given colume
plot(can.volume$empty.volume.column.raster)

# Plot the volume of the empty space within a 3x3 moving window
plot(can.volume$empty.canopy.volume.raster)

# Plot the volume of the euphotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.euphotic.raster)

# Plot the volume of the oligophotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.oligophotic.raster)

# Plot the total leaf area of the euphotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.euphotic.tla.raster)

# Plot the total leaf area of the oligophotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.oligophotic.tla.raster)

# Plot the top of canopy rugosity volume
plot(toc.rugos)

