plot_info <- read.csv('plotdata.csv')
agb <- raste('agb_raster.tif')

coordinates(plot_info) = ~ lon + lat
crs(plot_info) <- crs(agb)
Plot_agb <- raster::extract(agb, plot_info[,1:2])
