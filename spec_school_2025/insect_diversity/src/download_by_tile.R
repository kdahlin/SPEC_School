# Download NEON Lidar data for sites with beetle data and process forest metrics from lidar #
#### Load in packages ####
library(tidyverse)
library(sf)
library(lidR)
library(neonUtilities)

#### Set up sites ####
# Plots with Beetle data 
neon_beetle_plots <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots_2/')

# UTM zones for each plot
utm_zones <- tibble(siteID = c('YELL', 'ABBY', 'GRSM', 'MLBS', 'TREE', 'BART', 'HARV', 'ORNL', 'TALL'),
                    utm_crs = c(32612, 32610, 32617, 32617, 32616, 32619, 32618, 32616, 32616))

# From Jeffs Atkins Scripts. These are his custom forest metrics 
myMetrics <- function(z, rn, i, a, g, c){
  first  = rn == 1L
  zfirst = z[first]
  nfirst = length(zfirst)
  
  ground = c == 2
  zground = z[ground]
  nground = length(zground)
  
  firstground = rn == 1L & c == 2
  zfirstground = z[firstground]
  nfirstground = length(zfirstground)
  nz = length(z)
  
  metrics = list(
    # MeanAngle = mean(a),
    # MedAngle = median(a),
    # MaxAngle = max(a),
    # NoPoints = length(z),
    # NoGround = nground,
    # NoFirst = nfirst,
    # NoFirstGround =  nfirstground,
    iqr =  IQR(z), # inter-quartile range
    vci = VCI(z, zmax = max(z)), # vertical complexity index
    entropy = entropy(z, zmax = max(z)),
    fhd =   (entropy(z, zmax = max(z)) * log(max(z))),  #foliar height diversity
    vdr = ((max(z) - median(z)) / max(z)),
    top_rug = sd(zfirst),
    meanht = mean(z),
    medht = median(z),
    moch = mean(zfirst),
    # skew = (3 * (mean(z) - median(z)) / sd(z)), # Skew = 3 * (Mean – Median) / Standard Deviation
    # firstbelow1 = (sum(zfirst < 1) / nfirst) * 100,
    # firstbelow2 = (sum(zfirst < 2) / nfirst) * 100,
    # firstbelow5 = (sum(zfirst < 5) / nfirst) * 100,
    # total returns
    # below1 = (sum(z < 1) / nz) * 100,
    # below2 = (sum(z < 2) / nz) * 100,
    # below5 = (sum(z < 5) / nz) * 100,
    cc = 100 - ((sum(z < 2) / nz) * 100),
    crr = (mean(z) - min(z)) / (max(z) - min(z)),
    p5 = quantile(z, probs = 0.05),
    p10 = quantile(z, probs = 0.1),
    p25 = quantile(z, probs = 0.25),
    p50 = quantile(z, probs = 0.5),
    p75 = quantile(z, probs = 0.75),
    p90 = quantile(z, probs = 0.9),
    p95 = quantile(z, probs = 0.95),
    p99 = quantile(z, probs = 0.99))
  
  # return all them boys
  return(metrics)
}

# Also from Jeff's scripts 
process_lidar_file <- function(lidar_dat, res) {
  
  # normalize heights
  las.norm <- lidar_dat
  
  #las2 <- filter_poi(las.norm, Z >= 0 & Z < 50)
  # calculate the chm with the pitfree algorithm
  # Local maximum algorithm with a resolution of 1 meter replacing each
  # point by a 10 cm radius circle of 8 points and interpolating the empty
  # pixels using the 3-nearest neighbours and an inverse-distance weighting.
  #chm <- lidR::grid_canopy(las.norm, 1, algorithm = pitfree())
  
  
  #metrics = lasmetrics(lidar, myMetrics(Z, Intensity))
  
  # # remove big ol' files to clear memory
  xstart = -(res/2)
  ystart = -(res/2)
  # custom metrics function, note input variables and resolution, which is set at 10 as in 10 meters
  metrics <- lidR::grid_metrics(las.norm, ~myMetrics(z = Z, rn=ReturnNumber, i = Intensity, g = gpstime, c = Classification), 
                                start = c(xstart, ystart), res = res, filter = ~Z >= 0 & Z <= 75 )    
  
  # this renames them
  # stack.names = c("MeanAngle", "MedAngle", "MaxAngle", "NoPoints", "IQR", "VCI", "Entropy", "FHD", "VDR", "TopRugosity", "MeanHeight", "MedianHeight",
  #                 "MOCH", "Skewness", "FirstBelow1m", "FirstBelow2m", "FirstBelow5m",
  #                 "Below1m", "Below2m", "Below5m", "CC", "CRR", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
  stack.names <- c('iqr', 'vci', 'entropy', 'fhd', 'vdr', 'top_rug', 'meanht', 
                   'medht', 'moch', 'cc', 'crr', 'p5', 'p10', 'p25', 'p50', 
                   'p75', 'p90', 'p95', 'p99')
  
  names(metrics) <- stack.names
  
  return(metrics)
  
}


#### Download LiDAR data from NEON ####
# Loop through all sites and download all tiles over plots we want
# This take many hours on Mountin Lake Wifi
for(i in 1:nrow(utm_zones)) {

  # Filter to site
  neon_beetle_plots_this <- neon_beetle_plots %>%
    filter(siteID == utm_zones$siteID[i])

  # Get UTM zone and transform
  this_site <- utm_zones$siteID[i]

  utm_zones_this <- utm_zones %>%
    filter(siteID == !!this_site) %>%
    pull(utm_crs)

  neon_beetle_plots_this_t <- neon_beetle_plots_this %>%
    st_transform(., utm_zones_this) %>%
    mutate(geometry = st_centroid(geometry))

  # Get UTM easting and northing
  neon_beetle_plots_this_t$X <- st_coordinates(neon_beetle_plots_this_t)[,'X']
  neon_beetle_plots_this_t$Y <- st_coordinates(neon_beetle_plots_this_t)[,'Y']

  print(paste0('on ', utm_zones$siteID[i]))

  # Download lidar data
  byTileAOP(dpID="DP1.30003.001", site = utm_zones$siteID[i],
            year=2018, easting = neon_beetle_plots_this_t$X,
            northing = neon_beetle_plots_this_t$Y,
            buffer=1,
            check.size = F)

}

#### Calculate structure metrics for each plot ####

# Get all lidar files 
lidar_files <- list.files('DP1.30003.001/neon-aop-products/', 
                          recursive = TRUE,
                          full.names = TRUE)

lidar_files <- lidar_files[grep('[.]laz', lidar_files)]


# Loop though each file 
every_plot_summary <- tibble()
for(i in 1:length(lidar_files)){

  # Read file 
  lidar_data <- lidR::readLAS(lidar_files[i])
  
  # Make DEM
  dem <- lidR::grid_terrain(lidar_data, res = 5, algorithm = tin())
  
  # Make an extent polygon 
  neon_beetle_plots_utm <- neon_beetle_plots %>%
    st_transform(st_crs(dem))
  
  dem_extent <- dem
  
  terra::values(dem_extent) <- 1
  
  # Intersect plots and tiles 
  plot_tiles_intercetion <- st_intersects(st_as_sf(terra::as.polygons(terra::rast(dem_extent))), neon_beetle_plots_utm)
  
  # Match up plots to tile ids 
  intersecting_plots <- neon_beetle_plots[plot_tiles_intercetion[[1]],]
  
  intersecting_plots_t <- intersecting_plots %>%
    st_transform(st_crs(lidar_data))
  
  # loop though plots and process lidar
  all_plot_summary <- tibble()
  
  if(nrow(intersecting_plots_t) == 0) next
  for(t in 1:nrow(intersecting_plots_t)) {
    
    # Get one plot
    this_plot <- intersecting_plots_t[t,]
    
    # Process LiDAR
    ## Buffer to avoid edge effects of metrics 
    clipped_plot <- lidR::clip_roi(lidar_data, st_buffer(this_plot, 50))
    
    # Normalize to height (point cloud)
    normalized_point_cloud <- lidR::normalize_height(clipped_plot, dem, na.rm = T)
    
    # Filter out false values 
    normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=60)
    
    # Gernate forest metrics with Jeffs functions 
    forest_metrics <- try(process_lidar_file(lidar_dat = normalized_point_cloud_fil, 
                                         res = 5),
                          silent = TRUE)
    
    if(class(forest_metrics) %in% 'try-error') {
      print(paste0(this_plot$plotID, ' Feild, no points in plot'))
      next
    }
    
    # Save a raster brick for all metrics
    plot_metrics_brick <- raster::mask(forest_metrics, this_plot)
    
    raster::writeRaster(plot_metrics_brick, paste0('spec_school_2025/insect_diversity/data/plot_forest_metrics/', 
                                                   this_plot$plotID, '.tif'),
                        overwrite=TRUE)
    
    raster_layers <- raster::nlayers(plot_metrics_brick)
    
    # Create a table with the summarized metric for the plot 
    plot_summary <- tibble()
    for(s in 1:raster_layers) {
      
      layer_name <- names(plot_metrics_brick[[s]])
      
      value_mean <- mean(raster::values(plot_metrics_brick[[s]]), na.rm = TRUE)
      
      this_metric <- tibble(metric = !!layer_name,
                            value = value_mean,
                            plot = this_plot$plotID)
      
      plot_summary <- rbind(plot_summary, this_metric)
    }
    
    all_plot_summary <- rbind(plot_summary, all_plot_summary)
  }
  every_plot_summary <- rbind(every_plot_summary, all_plot_summary)
}


write_csv(every_plot_summary, 'spec_school_2025/insect_diversity/data/lidar_metrics_2.csv')

# look <- read_csv('spec_school_2025/insect_diversity/data/lidar_metrics.csv')
#### Find what plot goes in what tile ####
lidar_extent_files <- list.files('DP1.30003.001/neon-aop-products/2022/FullSite/', 
                                recursive = T,
                                full.names = T)

lidar_tile_extent <- lidar_extent_files[grep('kml', lidar_extent_files)]

all_tiles <- tibble()
for(i in 1:length(lidar_tile_extent)){
  
  this_tile_extent <- st_read(lidar_tile_extent[i]) %>%
    mutate(Name = str_extract(lidar_tile_extent[i], '[0-9]{6}_[0-9]{7}')) %>%
    sf::st_zm() %>%
    st_cast("POLYGON") 
  
  all_tiles <- rbind(all_tiles, this_tile_extent)
  
  
}


#### Look at a plot ####
# Site name we want 
site_name <- 'ABBY_002'

#All lidar file
lidar_files <- list.files('DP1.30003.001/neon-aop-products/2022/FullSite/',
                          recursive = T,
                          full.names = T)

# get just lidar 
lidar_files_laz <- lidar_files[grep('laz', lidar_files)]

# transform 
all_tiles <- all_tiles %>%
  st_transform(, st_crs(neon_beetle_plots))

# intersect plots and tiles 
intetsect <- st_intersects(neon_beetle_plots, all_tiles)

# get intersections 
site_intex <- grep(site_name, neon_beetle_plots$plotID)
file_index <- intetsect[[site_intex]]

file_name <- all_tiles$Name[file_index]

lidar_data <- lidR::readLAS(lidar_files_laz[grep(file_name, lidar_files_laz)])

this_plot <- neon_beetle_plots %>%
  filter(plotID == !!site_name)

clipped_plot <- lidR::clip_roi(lidar_data, st_buffer(st_transform(this_plot, st_crs(lidar_data)), 50))

# Make dem
dem <- lidR::grid_terrain(clipped_plot, res = 1, algorithm = tin())

# Normalize to height (point cloud)
normalized_point_cloud <- lidR::normalize_height(clipped_plot, dem, na.rm = T)

# Filter data 
normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=10)

# plot(normalized_point_cloud_fil)

clipped_plot_fin <- lidR::clip_roi(normalized_point_cloud_fil, st_transform(this_plot, st_crs(lidar_data)))

plot(clipped_plot_fin, pal = viridis::viridis(n = 10))
# Low: ABBY_002
# High: GRSM_008
#### Find what plot goes in what tile ####
lidar_extent_files <- list.files('DP1.30003.001/neon-aop-products/2022/FullSite/', 
                                 recursive = T,
                                 full.names = T)

lidar_tile_extent <- lidar_extent_files[grep('kml', lidar_extent_files)]

all_tiles <- tibble()
for(i in 1:length(lidar_tile_extent)){
  
  this_tile_extent <- st_read(lidar_tile_extent[i]) %>%
    mutate(Name = str_extract(lidar_tile_extent[i], '[0-9]{6}_[0-9]{7}')) %>%
    sf::st_zm() %>%
    st_cast("POLYGON") 
  
  all_tiles <- rbind(all_tiles, this_tile_extent)
  
  
}


#### OTHER STUFF ####
# Not sure what all this is, old code mostly and plotting 
# Site name we want 
site_name <- 'GRSM_008'

#All lidar file
lidar_files <- list.files('DP1.30003.001/neon-aop-products/2022/FullSite/',
                          recursive = T,
                          full.names = T)

# get just lidar 
lidar_files_laz <- lidar_files[grep('laz', lidar_files)]

# transform 
all_tiles <- all_tiles %>%
  st_transform(, st_crs(neon_beetle_plots))

# intersect plots and tiles 
intetsect <- st_intersects(neon_beetle_plots, all_tiles)

# get intersections 
site_intex <- grep(site_name, neon_beetle_plots$plotID)
file_index <- intetsect[[site_intex]]

file_name <- all_tiles$Name[file_index]

lidar_data <- lidR::readLAS(lidar_files_laz[grep(file_name, lidar_files_laz)])

this_plot <- neon_beetle_plots %>%
  filter(plotID == !!site_name)

clipped_plot <- lidR::clip_roi(lidar_data, st_buffer(st_transform(this_plot, st_crs(lidar_data)), 50))

# Make dem
dem <- lidR::grid_terrain(clipped_plot, res = 1, algorithm = tin())

# Normalize to height (point cloud)
normalized_point_cloud <- lidR::normalize_height(clipped_plot, dem, na.rm = T)

# Filter data 
normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=10)

# plot(normalized_point_cloud_fil)

clipped_plot_fin <- lidR::clip_roi(normalized_point_cloud_fil, st_transform(this_plot, st_crs(lidar_data)))

plot(clipped_plot_fin, pal = viridis::viridis(n = 10))
# Low: ABBY_002
# High: GRSM_008

# ABBY_002_lidar <- clipped_plot_fin
# GRSM_008_lidar <- clipped_plot_fin

abby_d2 <- tibble(x = ABBY_002_lidar@data[["X"]],
                  y = ABBY_002_lidar@data[["Z"]],
                  z = ABBY_002_lidar@data[["Z"]],
                  site = 'ABBY \n CRR: 0.32')

grsm_d2 <- tibble(x = GRSM_008_lidar@data[["X"]],
                  y = GRSM_008_lidar@data[["Z"]],
                  z = GRSM_008_lidar@data[["Z"]],
                  site = 'GRSM \n CRR: 0.75')

d2 <- rbind(abby_d2, grsm_d2)
ggplot(d2, aes(y = z,
               x = x,
               color = z)) +
  geom_point() +
  theme_few() +
  viridis::scale_color_viridis() +
  facet_wrap(~site, scales = 'free_x')


