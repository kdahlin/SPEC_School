#### Plot Lidar data ####
# THis code is a mess, sorry!

library(tidyverse)
library(sf)
library(lidR)
library(ggthemes)
library(mapview)
library(neonUtilities)
sf_use_s2(FALSE)

lidar_metrics_1 <- read_csv('spec_school_2025/insect_diversity/data/lidar_metrics.csv')
lidar_metrics_2 <- read_csv('spec_school_2025/insect_diversity/data/lidar_metrics_2.csv')
lidar_metrics <- rbind(lidar_metrics_1, lidar_metrics_2) %>%
  group_by(plot, metric) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')
#### Set up sites ####
# Plots with Beetle data 
sites_of_interest <- c('GRSM_020', 'BART_068', 'ORNL_027', 'ABBY_002', 'GRSM_008', 'TALL_003', 'ORNL_021', 'MLBS_008', 
                       'ABBY_010', 'BART_079', 'TALL_009')

neon_beetle_plots_2 <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots_2')
neon_beetle_plots_1 <- st_read('spec_school_2025/insect_diversity/data/NEON_Beetle_Plots')

neon_beetle_plots <- rbind(neon_beetle_plots_1, neon_beetle_plots_2)

look <- neon_beetle_plots %>%
  filter(plotID %in% c('ABBY_002', 'ABBY_010'))

mapview::mapview(neon_beetle_plots)

# UTM zones for each plot
utm_zones <- tibble(siteID = c('YELL', 'ABBY', 'GRSM', 'MLBS', 'TREE', 'BART', 'HARV', 'ORNL', 'TALL'),
                    utm_crs = c(32612, 32610, 32617, 32617, 32616, 32619, 32618, 32616, 32616))



#### Look at a plot ####
# pull tile for a site of interest 
lidar_extent_files <- list.files('DP1.30003.001/neon-aop-products/', 
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
#All lidar file
lidar_files <- list.files('DP1.30003.001/neon-aop-products/',
                          recursive = T,
                          full.names = T)

# get just lidar 
lidar_files_laz <- lidar_files[grep('laz', lidar_files)]

# transform 
all_tiles <- all_tiles %>%
  st_transform(, st_crs(neon_beetle_plots))

# intersect plots and tiles 
intetsect <- st_intersects(neon_beetle_plots, all_tiles)

utm_zones <- tibble(siteID = c('YELL', 'ABBY', 'GRSM', 'MLBS', 'TREE', 'BART', 'HARV', 'ORNL', 'TALL'),
                    utm_crs = c(32612, 32610, 32617, 32617, 32616, 32619, 32618, 32616, 32616))


all_x_y_data <- tibble()
for(i in 1:length(sites_of_interest)){
  
  site_name <- sites_of_interest[i]
  
  utm_zone <- utm_zones %>%
    filter(siteID == str_split_fixed(site_name, '_', n = Inf)[,1]) %>%
    pull(utm_crs)
  
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
  normalized_point_cloud_fil <- filter_poi(normalized_point_cloud, Z>=0, Z<=50)
  
  # plot(normalized_point_cloud_fil)
  
  clipped_plot_fin <- lidR::clip_roi(normalized_point_cloud_fil, st_transform(this_plot, st_crs(lidar_data)))
  plot(clipped_plot_fin)
  #### Plot Transect ####
  coord.ref <- st_crs(paste0("EPSG:", utm_zone))
  
  points.sf <- neon_beetle_plots %>%
    # filter(plotID == 'GRSM_008') 
    filter(plotID == !!site_name) 
  
  # and let's reproject our point location then round to the thousands
  utm.coords <- st_transform(points.sf,
                             crs = coord.ref) %>%
    mutate(geometry = st_centroid(geometry))
  
  corner.easting <- unlist(utm.coords$geometry)[1]
  corner.northing <- unlist(utm.coords$geometry)[2]
  p1 <- c(corner.easting + 10, corner.northing + 10)
  p2 <- c(corner.easting + 150, corner.northing + 150)
  
  transect_grsm <- clip_transect(normalized_point_cloud_fil, p1, p2, width = 5, xz = TRUE)
  
  # plot with ggplot (payload is a lidR function to transform las to coordinates)
  ggplot(payload(transect), aes(X,Z, color = Z)) + 
    geom_point(size = 0.5) + 
    coord_equal() + 
    theme_minimal() +
    viridis::scale_color_viridis() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = 'none') +
    labs(y = 'Height (m)',
         title = 'TALL')
  
  # fin_tib <- tibble(x = clipped_plot_fin@data[["X"]],
  #                   y = clipped_plot_fin@data[["Z"]],
  #                   z = clipped_plot_fin@data[["Z"]],
  #                   site = !!site_name)
  # 
  # all_x_y_data <- rbind(all_x_y_data, fin_tib)
  
}

png('abby_transect.png')
ggplot(payload(transect_abby), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  viridis::scale_color_viridis() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)',
       title = 'Abby Road')
dev.off()

png('GRSM_transect.png')
ggplot(payload(transect_grsm), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  viridis::scale_color_viridis() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)',
       title = 'Great Smoky Mountains')
dev.off()

unique(all_x_y_data$site)


ab <- all_x_y_data %>%
  # filter(site == 'ABBY_002') %>%
  group_by(site) %>%
  summarise(mean = mean(z),
            max = max(z),
            min = min(z))

grsm <- all_x_y_data %>%
  filter(site == 'GRSM_008') %>%
  ggplot(., aes(y = z,
               x = x,
               color = z)) +
  geom_point() +
  ylim(0,40) +
  theme_few() +
  # geom_hline(yintercept = 4) +
  viridis::scale_color_viridis(limits = c(0, 40)) +
  # viridis::scale_color_viridis() +
  geom_hline(yintercept = 25.2966528, color = 'red') +
  geom_hline(yintercept = 0, color = 'red') +
  geom_hline(yintercept = 38.150, color = 'red') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)',
       title = 'Smoky Mountains')

# BART
# TALL
fin_tib %>%
  filter(site == '')
  ggplot(., aes(y = z,
                x = x,
                color = z)) +
  geom_point() +
  # ylim(0,40) +
  theme_few() +
  # geom_hline(yintercept = 4) +
  viridis::scale_color_viridis() +
  # viridis::scale_color_viridis() +
  # geom_hline(yintercept = 25.2966528, color = 'red') +
  # geom_hline(yintercept = 0, color = 'red') +
  # geom_hline(yintercept = 38.150, color = 'red') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)',
       title = 'MLBS')
  
  # extract a transect
  # now let's look at a cross section
  # first define the start and end of the transect (in UTM coordinates)
  # you should change 10 to your utm_number
  


abby <- all_x_y_data %>%
  filter(site == 'ABBY_002') %>%
  ggplot(., aes(y = z,
                x = x,
                color = z)) +
  geom_point() +
  ylim(0,40) +
  theme_few() +
  geom_hline(yintercept = 2.075862, color = 'red') +
  geom_hline(yintercept = 0, color = 'red') +
  geom_hline(yintercept = 8.132, color = 'red') +
  viridis::scale_color_viridis(limits = c(0, 40)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)',
       title = 'Abby Road')

unique(all_x_y_data$site)

all_x_y_data %>%
  filter(site == 'TALL_003') %>%
  ggplot(., aes(y = z,
                x = x,
                color = z)) +
  geom_point() +
  # ylim(0,10) +
  theme_few() +
  geom_hline(yintercept = 5.3202278, color = 'red') +
  geom_hline(yintercept = 0, color = 'red') +
  geom_hline(yintercept = 31.402, color = 'red') +
  viridis::scale_color_viridis() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)')
      #  title = 'Abby Road')

all_x_y_data %>%
  filter(site == 'ABBY_002') %>%
  ggplot(., aes(y = z,
                x = x,
                color = z)) +
  geom_point() +
  # ylim(0,10) +
  theme_few() +
  geom_hline(yintercept = 5.3202278, color = 'red') +
  geom_hline(yintercept = 0, color = 'red') +
  geom_hline(yintercept = 31.402, color = 'red') +
  viridis::scale_color_viridis() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  labs(y = 'Height (m)')


ggpubr::ggarrange(abby, grsm)



# Etorpy / sqr(max_height)