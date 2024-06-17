################################################################################
### extracting reflectance data for each GPS location (with multiple flightlines
### per GPS location) ###
### Drafted by Kyla Dahlin (kdahlin@msu.edu)
### last updated 20240616
################################################################################



library(rhdf5)
library(raster)
library(rgdal)
options(digits = 20)

# this code will produce a csv with a row for each point x flightline combo
# where do you want to save that info?
out.dir <- "C:/Users/kdahlin/Dropbox/NEON_hsi_lidar/hsi_lidar_ms1/data/"
today <- format(Sys.Date(), "%Y%m%d")
loc <- "HARV"

# where to find stuff
hsi.data.dir <- paste0("X:/shared_data/NEON_AOP_data/",loc,"/2017/HSI/")

# get the file produced in the prev r script
pointsXflights <- read.csv(paste0(out.dir,loc,"_pointsXflights_20181120.csv"), 
                           stringsAsFactors = FALSE)

flights <- unique(pointsXflights$flightline)

# list all of the data types within the hdf5 (this should be the same for all files)
h5ls(file = paste0(hsi.data.dir, flights[1]))


# get a list of wavelengths (should be the same for all files)
wavelengths <- h5read(file = paste0(hsi.data.dir, flights[1]),
                      name = paste0(loc, 
                                    "/Reflectance/Metadata/Spectral_Data/Wavelength"))
wave.count <- 1:length(wavelengths)
wave.round <- round(wavelengths,0)
wave.names <- paste0("wave.", wave.round)

# make a quick table of the wavelength values for plotting later
out.waves <- as.data.frame(cbind(wave.count, wave.names, wavelengths))

write.csv(out.waves, 
          paste0(out.dir, "NEON_HSI_wavelengths_", today, ".csv"),
          row.names = FALSE)

# make an output table
out.data <- as.data.frame(matrix(data = NA, 
                                 nrow = 0, 
                                 ncol = (length(wave.names) + 4)), 
                                 stringsAsFactors = FALSE)

names(out.data) <- c("pointIDs", 
                     "easting", 
                     "northing", 
                     "flightline", 
                     wave.names)

for (i in 1:length(flights)) {
  # get the coordinate system
  coords <- h5read(file = paste0(hsi.data.dir, flights[i]),
                   name = paste0(loc, "/Reflectance/Metadata/Coordinate_System"))
  
  map.info <- strsplit(coords$Map_Info, 
                       split = ",", 
                       fixed = T)
  easting.ul <- as.numeric(map.info[[1]][4])
  northing.ul <- as.numeric(map.info[[1]][5])
  
  locs.subset <- subset(pointsXflights, 
                        pointsXflights$flightline == flights[i])
  locs.subset <- locs.subset[,-5]
  
  out.subset <- as.data.frame(matrix(data = NA, 
                                     nrow = dim(locs.subset[1]), 
                                     ncol = (length(wave.names) + 4)), 
                              stringsAsFactors = FALSE)
  names(out.subset) <- c("pointIDs", 
                         "easting", 
                         "northing", 
                         "flightline", 
                         wave.names)
  
  out.subset[,1:4] <- locs.subset
  
  # now open up each refl band and extract points
  for (j in 1:length(wavelengths)) {
    # get subsetted reflectance
    refl.sub <- h5read(file = paste0(hsi.data.dir, flights[i]),
                       name = paste0(loc, "/Reflectance/Reflectance_Data"),
                       index = list(j, NULL, NULL))
    
    refl.stack <- raster(as.matrix(refl.sub[1,,]))
    
    refl.stack[refl.stack == -9999] <- NA
    refl.stack.t <- t(refl.stack)
    
    line.dim <- dim(refl.stack.t)
    y.dim <- line.dim[1]
    x.dim <- line.dim[2]
    
    extent(refl.stack.t) <- c(easting.ul, 
                              easting.ul+x.dim, 
                              northing.ul-y.dim, 
                              northing.ul)
    
    # assign projection (note - this is hard coded because it's 'UTM' in the hdf5 
    #file and it's acutally case sensitive and should be 'utm' :[ sigh.)
    projection(refl.stack.t) <- "+proj=utm +zone=18 +north 
                                 +datum=WGS84 +units=m +no_defs"
    
    out.vals <- extract(refl.stack.t, 
                        as.matrix(out.subset[,2:3]), 
                        method = "simple")
    
    out.subset[,4+j] <- out.vals
    print(paste(j, date()))
  }

  out.data <- rbind(out.data, out.subset)
  print(paste("done with ", i, date()))
}

out.nas <- is.na(out.data$wave.384) == F

out.keep <- subset(out.data, out.nas)

write.csv(out.keep, 
          paste0(out.dir, loc, "_hsi_by_point_flightline_", today, ".csv"),
          row.names = FALSE)


### making a plot just to make me happy

x11()
plot(wave.round, c(0, rep(10000, length(wave.round)-1)), type = "n")
lines(wave.round, out.keep[25,5:430])
lines(wave.round, out.keep[150,5:430])
lines(c(397,397), c(0,10000), col = "red")
lines(c(1352,1352), c(0,10000), col = "red")
lines(c(1428,1428), c(0,10000), col = "red")
lines(c(1808,1808), c(0,10000), col = "red")
lines(c(1963,1963), c(0,10000), col = "red")
lines(c(2473,2473), c(0,10000), col = "red")


