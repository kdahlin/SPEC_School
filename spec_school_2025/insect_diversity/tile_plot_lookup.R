library(tidyverse)
library(sf)
library(mapview)
library(neonUtilities)
library(raster)
sf_use_s2(F)


##### Read in plot data and filter to site ####
neon_gps <- st_read('data/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Polygons_V11.shp')

# Just looking at mountin lake for now
mlbs_plots <- neon_gps %>%
  filter(siteID == 'MLBS',
         subtype == 'basePlot') 
mapview(mlbs_plots)

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

# Tile polygon path 
tiles <- list.files('DP3.30015.001/neon-aop-products/2022/FullSite/D07/2022_MLBS_5/Metadata/DiscreteLidar/TileBoundary/kmls/', full.names = T)

all_tiles <- tibble()
for(i in 1:length(tiles)){
  print(tiles[i])
  this_tile <- st_read(tiles[i]) %>%
    mutate(Name = str_split_fixed(tiles[i], '/', n = Inf)[1,12]) %>%
    sf::st_zm() %>%
    st_cast("POLYGON") 
  all_tiles <- rbind(all_tiles, this_tile)
}

# Convert plots to tiles CRS
mlbs_plots_ <- mlbs_plots %>%
  st_transform(st_crs(all_tiles))

# inerest plots and tiles 
plot_tiles_intercetion <- st_intersects(mlbs_plots_, all_tiles)

# Match up plots to tile ids 
plot_tile_look_up <- tibble()
for(i in 1:length(plot_tiles_intercetion)){
  plot_id <- mlbs_plots[[i,1]]
  
  file_name <- try(all_tiles[[plot_tiles_intercetion[[i]],1]],
                   silent = TRUE)
  
  if(class(file_name) %in% 'try-error'){
    file_name <- NA
  }
  
  this_look_up <- tibble(plot_id = !!plot_id,
                         tile = !!file_name)
  
  plot_tile_look_up <- rbind(plot_tile_look_up, this_look_up)
}

# Pull out tile numbers 
t=1
plot_tile_look_up <- plot_tile_look_up %>%
  mutate(tile_number = str_extract(tile, '[0-9]{6}_[0-9]{6}'))




plot_with_beele <- paste0('MLBS_', c('001', '002', '003', '005', '006', '007', '008', '009', '010', '011'))

canopy_height_files <- list.files('DP3.30015.001/neon-aop-products/2022/FullSite/D07/2022_MLBS_5/L3/DiscreteLidar/CanopyHeightModelGtif/', full.names = T)

# canopy_height_files[grep(plot_tile_look_up[t, 3], canopy_height_files)]

dir.create('data/croped_canopy_hieght')
for(i in 1:nrow(plot_tile_look_up)) {
  
  if(is.na(plot_tile_look_up[i, 3])) next
  
  this_tile <- terra::rast(canopy_height_files[grep(plot_tile_look_up[i, 3], canopy_height_files)])
  this_plot <- mlbs_plots %>%
    filter(plotID == plot_tile_look_up[[i,1]]) %>%
    st_transform(., st_crs(this_tile))
  
  cropped_tile <- crop(this_tile, this_plot)
  
  # mapview(raster(canopy_height_files[grep(plot_tile_look_up[i, 3], canopy_height_files)])) + this_plot
  
  # mapview(raster::raster(cropped_tile)) + this_plot
  
  terra::writeRaster(cropped_tile, paste0('data/croped_canopy_hieght/', this_plot$plotID, '.tif'),
                     overwrite=TRUE)

}

look <- raster('data/croped_canopy_hieght/MLBS_002.tif')
look_ <- raster('data/croped_canopy_hieght/MLBS_006.tif')
mapview(look_) 
