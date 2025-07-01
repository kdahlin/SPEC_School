# THis script is old and not needed anymore.

# https://stackoverflow.com/questions/66011929/package-rgl-in-r-not-loading-in-mac-os/66127391#66127391

library(tidyverse)
library(sf)
library(mapview)
library(neonUtilities)
library(raster)
library(lidR)
sf_use_s2(F)


##### Read in plot data and filter to site ####
neon_gps <- st_read('spec_school_2025/insect_diversity/data/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Polygons_V11.shp')
neon_beetle_plots <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots/')


sites <- c('MLBS', 'TREE', 'GRSM', 'YELL', 'ABBY')
# Just looking at mountin lake for now

all_plots <- neon_gps %>%
  filter(siteID %in% !!sites,
         subtype == 'basePlot') %>%
  filter(plotID %in% neon_beetle_plots$plotID)
mapview(all_plots)

st_write(all_plots, 'spec_school_2025/insect_diversity/data/base_plots',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)

look <- all_plots %>%
  filter(plotID == 'GRSM_006')
mapview(look) + all_tiles

# Jeffs Forest Metrics Functions 
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
  rm(las)
  xstart = -(res/2)
  ystart = -(res/2)
  # custom metrics function, note input variables and resolution, which is set at 10 as in 10 meters
  metrics <- lidR::grid_metrics(las.norm, ~myMetrics(z = Z, rn=ReturnNumber, i = Intensity, g = gpstime, c = Classification), 
                                start = c(xstart, ystart), res = res, filter = ~Z >= 0 & Z <= 75 )    
  
  # this renames them
  stack.names = c("MeanAngle", "MedAngle", "MaxAngle", "NoPoints", "IQR", "VCI", "Entropy", "FHD", "VDR", "TopRugosity", "MeanHeight", "MedianHeight",
                  "MOCH", "Skewness", "FirstBelow1m", "FirstBelow2m", "FirstBelow5m",
                  "Below1m", "Below2m", "Below5m", "CC", "CRR", "p10", "p25", "p50", "p75", "p90", "p95", "p99")


  names(metrics) <- stack.names
  
  return(metrics)
  
}


#### Downlaod Data ####
# sites <- c('MLBS', 'TREE', 'GRSM', 'ABBY', 'YELL')
# byFileAOP("DP1.30001.001", site="MLBS",
#           year=2022, check.size=T)
# byFileAOP("DP3.30015.001", site=c('MLBS'),
#           year=2022, check.size=T)
# byFileAOP("DP3.30015.001", site=c('TREE'),
#           year=2022, check.size=T)
# byFileAOP("DP3.30015.001", site=c('MLBS'),
#           year=2022, check.size=T)

#### subset to plots ####
# Extract the tile id for each plot ID

domains <- list.files('spec_school_2025/insect_diversity/data/DP3.30015.001/neon-aop-products/2022/FullSite')
# Tile polygon path 
for(i in 1:length(domains)){
  
  domain <- domains[i]
  sites_in_domain <- list.files(paste0('spec_school_2025/insect_diversity/data/DP3.30015.001/neon-aop-products/2022/FullSite/', domain))
  
  for(t in 1:length(sites_in_domain)){
    
    this_site <- sites_in_domain[t]
    
    tiles <- list.files(paste0('spec_school_2025/insect_diversity/data/DP3.30015.001/neon-aop-products/2022/FullSite/', domain, '/', this_site, '/Metadata/DiscreteLidar/TileBoundary/kmls/'), full.names = T)
    
    all_tiles <- tibble()
    for(s in 1:length(tiles)){
      # print(tiles[s])
      this_tile <- st_read(tiles[s]) %>%
        mutate(Name = str_extract(tiles[s], '[0-9]{6}_[0-9]{7}')) %>% # str_split_fixed(tiles[i], '/', n = Inf)[1,12]) %>%
        sf::st_zm() %>%
        st_cast("POLYGON") 
      all_tiles <- rbind(all_tiles, this_tile)
    }
    
    site_name <- str_extract(this_site, '[A-Z]{4}')
    # Convert plots to tiles CRS
    if(this_site %in% c('2022_CHEQ_7', '2022_STEI_5')) {
      site_name <- 'TREE'
    }
    
    these_plots <- all_plots %>%
      filter(siteID == !!site_name) %>%
      st_transform(st_crs(all_tiles))
    
    # inerest plots and tiles 
    plot_tiles_intercetion <- st_intersects(these_plots, all_tiles)
    
    # Match up plots to tile ids 
    plot_tile_look_up <- tibble()
    for(p in 1:length(plot_tiles_intercetion)){
      plot_id <- these_plots[[p,1]]
      
      file_name <- try(all_tiles[[plot_tiles_intercetion[[p]],1]],
                       silent = TRUE)
      
      if(class(file_name) %in% 'try-error'){
        file_name <- NA
      }
      
      this_look_up <- tibble(plotID = !!plot_id,
                             plotID_number = !!this_site,
                             tile = !!file_name)
      
      plot_tile_look_up <- rbind(plot_tile_look_up, this_look_up)
    }
    
    if(all(is.na(plot_tile_look_up$tile))) next
    
    write_csv(plot_tile_look_up, paste0('spec_school_2025/insect_diversity/data/tile_look_ups/', this_site, '_tile_lookup.csv'))
  }
}

all_fils <- list.files('spec_school_2025/insect_diversity/data/tile_look_ups/', full.names = TRUE)

neon_beetle_plots <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots/')
all_tables <- map_dfr(all_fils, read_csv) %>%
  filter(plotID %in% neon_beetle_plots$plotID) %>%
  filter(!is.na(tile))

look <- all_tables %>%
  group_by(plotID) %>%
  summarise(n = n())

write_csv(all_tables, 'spec_school_2025/insect_diversity/data/tile_lookups_2.csv')

# Pull out tile numbers 
t=1

# plot_with_beele <- paste0('MLBS_', c('001', '002', '003', '005', '006', '007', '008', '009', '010', '011'))

canopy_height_files <- list.files('spec_school_2025/insect_diversity/data/DP3.30015.001/neon-aop-products/2022/FullSite/D07/2022_MLBS_5/L3/DiscreteLidar/CanopyHeightModelGtif/', full.names = T)

# canopy_height_files[grep(plot_tile_look_up[t, 3], canopy_height_files)]

dir.create('spec_school_2025/insect_diversity/data/croped_canopy_hieght')
for(i in 1:nrow(plot_tile_look_up)) {
  
  if(is.na(plot_tile_look_up[i, 2])) next
  
  this_tile <- terra::rast(canopy_height_files[grep(plot_tile_look_up[i, 2], canopy_height_files)])
  this_plot <- mlbs_plots %>%
    filter(plotID == plot_tile_look_up[[i,1]]) %>%
    st_transform(., st_crs(this_tile))
  
  cropped_tile <- crop(this_tile, this_plot)
  
  # mapview(raster(canopy_height_files[grep(plot_tile_look_up[i, 3], canopy_height_files)])) + this_plot
  
  # mapview(raster::raster(cropped_tile)) + this_plot
  
  terra::writeRaster(cropped_tile, paste0('spec_school_2025/insect_diversity/data/croped_canopy_hieght/', this_plot$plotID, '.tif'),
                     overwrite=TRUE)
  
}


# Downlaod 
plots <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots')

# Subset lidar to plot 
plot_lookup <- read_csv('spec_school_2025/insect_diversity/data/tile_lookups.csv') %>%
  filter(plotID %in% plots$plotID) %>%
  filter(plotID != 'MLBS_010')
# mutate(tile = paste0(tile, 0)) 
# filter(plot_id %in% plots$plotID)

# downlaod lidar data 
for(i in 1:2){
  
  plot <- plot_lookup$plotID[i]
  site <- str_extract(plot, '[A-Z]{4}')
  tile <- plot_lookup$tile[i]
  
  if(is.na(tile)) next
  tile_northing <- str_split_fixed(tile, '_', n = Inf)[,1]
  tile_easting <- str_split_fixed(tile, '_', n = Inf)[,2]
  
  
  # plot_lidar
  byTileAOP(dpID = "DP1.30003.001",
            site = site,
            year = 2022,
            easting = tile_easting,
            northing = tile_northing,
            buffer = 0,
            check.size = FALSE,
            savepath = 'spec_school_2025/insect_diversity/data')
}

# all_fils <- list.files('spec_school_2025/insect_diversity/data/DP1.30003.001/neon-aop-products/2022/FullSite/', recursive = T, full.names = T)
# files_we_want <- all_fils[str_detect(all_fils, '[.]laz')]
# 
# file.copy(files_we_want, 'spec_school_2025/insect_diversity/data/lidar_all_sites/')
# 
# paste0('spec_school_2025/insect_diversity/data/lidar_all_sites/', str_split_fixed(files_we_want, '/', n = Inf)[,6])

# look <- raster('spec_school_2025/insect_diversity/data/croped_canopy_hieght/MLBS_002.tif')
# look_ <- raster('spec_school_2025/insect_diversity/data/croped_canopy_hieght/MLBS_006.tif')
# mapview(look_) 

#### Subset tiles to plots ####
all_tiles <- list.files('spec_school_2025/insect_diversity/data/lidar_sar_download', full.names = TRUE)

for(i in 8:nrow(plot_lookup)){
  tile <- plot_lookup[[i,3]]
  plot <- plot_lookup[[i,1]]
  
  las_file <- all_tiles[grep(tile, all_tiles)]
  
  if(any(is.na(las_file))) next
  
  this_tile <- readLAS(las_file)
  
  this_plot <- all_plots %>%
    filter(plotID == !!plot) %>%
    st_transform(., st_crs(this_tile))
  
  cliped_lidar_data <- lidR::clip_roi(this_tile, this_plot)
  
  # Calculate DEM
  dem <- lidR::grid_terrain(cliped_lidar_data, res = 5, algorithm = tin())
  
  # Normalize to height (point cloud)
  normalized_point_cloud <- lidR::normalize_height(cliped_lidar_data, dem, na.rm = T)
  
  lidR::writeLAS(normalized_point_cloud, paste0('spec_school_2025/insect_diversity/data/clipped_laz/', plot, '.laz'))
  
}

neon_beetle_plots_ <- neon_beetle_plots %>%
  filter(str_detect(plotID, 'MLBS')) %>%
  mutate(geometry = st_centroid(geometry)) %>%
  st_transform(32617) 

neon_beetle_plots_$X <- st_coordinates(neon_beetle_plots_)[,'X']
neon_beetle_plots_$Y <- st_coordinates(neon_beetle_plots_)[,'Y']

byTileAOP(dpID="DP1.30003.001", site="MLBS", 
          year=2022, easting = neon_beetle_plots_$X[4],
          northing = neon_beetle_plots_$Y[4],
          buffer=1)


look=getProductInfo('DP1.30003.001')
neonUtilities::byFileAOP()

this_tile <- readLAS('DP1.30003.001/neon-aop-products/2022/FullSite/D12/2022_YELL_4/L1/DiscreteLidar/ClassifiedPointCloud/NEON_D12_YELL_DP1_529000_4977000_classified_point_cloud_colorized.laz')
# tiles <- list.files(paste0('spec_school_2025/insect_diversity/data/DP3.30015.001/neon-aop-products/2022/FullSite/', 'D05', '/', '2022_STEI_5', '/Metadata/DiscreteLidar/TileBoundary/kmls/'), full.names = T)
# exten_fil <- tiles[grep('299000_5040000', tiles)]
# exten <- st_read(exten_fil) %>%
#   mutate(Name = 'g',
#          Description = 'g') %>%
#   sf::st_zm() %>%
#   st_cast("POLYGON") 
# mapview(exten) + dem

# Subset plots to plot in this tile 
# this_plot <- all_plots %>%
#   filter(plotID %in% c('MLBS_011')) %>%
#   st_transform(., st_crs(this_tile))

# Clip lidar data to plot
# this_plot_centroid <- st_centroid(this_plot)

# cliped_lidar_data <- lidR::clip_roi(this_tile, 
#                                     geometry = st_bbox(this_plot))
# plot(cliped_lidar_data)

# plot(this_tile)
# Calculate DEM
dem <- lidR::grid_terrain(this_tile, res = 5, algorithm = tin())
# mapview(dem)
# Normalize to height (point cloud)
normalized_point_cloud <- lidR::normalize_height(this_tile, dem, na.rm = T)


normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=100)
lidR::plot(normalized_point_cloud_fil)


library(lidR)
library(rgl)

las <- readLAS('DP1.30003.001/neon-aop-products/2022/FullSite/D07/2022_GRSM_7/L1/DiscreteLidar/ClassifiedPointCloud/NEON_D07_GRSM_DP1_275000_3951000_classified_point_cloud_colorized.laz')
dem <- lidR::grid_terrain(las, res = 5, algorithm = tin())
# mapview(dem)
# Normalize to height (point cloud)
normalized_point_cloud <- lidR::normalize_height(las, dem, na.rm = T)


normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=100)

# exportPath = tempfile()
plot(normalized_point_cloud_fil, pal = viridis::viridis(n = 10))
# snapshot3d('test.png')
# movie3d(spin3d(), duration = 5, movie = exportPath)
# Grid metrics (more complicated part)

# grid_metrics <- lidR::grid_metrics(readLAS('spec_school_2025/insect_diversity/data/clipped_laz/ABBY_006.laz'), .stdmetrics, 1)

# mapview(grid_metrics$zmax)


#### PLAY ####
#### Look at a plot ####
# Site name we want 
site_name <- 'ABBY_004'

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
# ABBY_004_lidar <- clipped_plot_fin

abby_d2 <- tibble(x = ABBY_002_lidar@data[["X"]],
                  y = ABBY_002_lidar@data[["Z"]],
                  z = ABBY_002_lidar@data[["Z"]],
                  site = 'ABBY \n CRR: 0.32') %>%
  mutate(mean = mean(z),
         min = min(z),
         max = max(z))

abby_4_d2 <- tibble(x = ABBY_004_lidar@data[["X"]],
                  y = ABBY_004_lidar@data[["Z"]],
                  z = ABBY_004_lidar@data[["Z"]],
                  site = 'ABBY_4 \n CRR: 0.35') %>%
  mutate(mean = mean(z),
         min = min(z),
         max = max(z))

grsm_d2 <- tibble(x = GRSM_008_lidar@data[["X"]],
                  y = GRSM_008_lidar@data[["Z"]],
                  z = GRSM_008_lidar@data[["Z"]],
                  site = 'GRSM \n CRR: 0.75') %>%
  mutate(mean = mean(z),
         min = min(z),
         max = max(z))

# (mean(x) - min(x)) / (max(x) - min(x))

d2 <- rbind(abby_d2, grsm_d2, abby_4_d2)
ggplot(d2, aes(y = z,
               x = x,
               color = z)) +
  geom_hline(yintercept = mean) +
  geom_point() +
  theme_few() +
  viridis::scale_color_viridis() +
  facet_wrap(~site, scales = 'free_x')


