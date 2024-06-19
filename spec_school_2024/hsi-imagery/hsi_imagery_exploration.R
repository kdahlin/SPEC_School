################################################################################
### looking at reflectance data tiles and making some plots
### Drafted by Kyla Dahlin (kdahlin@msu.edu)
### modified from hypRspec (Kamoske)
### last updated 20240616
################################################################################
#install.packages("BiocManager")

library(BiocManager)
BiocManager::install("rhdf5")

library(rhdf5)
library(terra)
options(digits = 20)

# this code will produce a csv with a row for each point x flightline combo
# where do you want to save that info?
out.dir <- "K:/SPEC_School_2024/HSI_image_practice/"
today <- format(Sys.Date(), "%Y%m%d")
loc <- "MLBS"

# where to find stuff
hsi.data.dir <- paste0("Y:/shared_data/NEON_AOP_data/MLBS/2022/NEON_refl-surf-",
                       "bidir-ortho-mosaic/NEON.D07.MLBS.DP3.30006.002.2022-09.",
                       "basic.20240530T173638Z.PROVISIONAL/")

# get hsi filenames
hsi.files <- list.files(hsi.data.dir)

# list all of the data types within the hdf5 (should be the same for all files)
h5ls(file = paste0(hsi.data.dir, hsi.files[3]))

# get a list of wavelengths (should be the same for all files)
wavelengths <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
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

# let's get the projection info too
in.proj <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                      name = paste0("/MLBS/Reflectance/Metadata/",
                      "Coordinate_System/EPSG Code"))

# and let's fine the lower left easting and northing values
in.info <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                  name = paste0("/MLBS/Reflectance/Metadata/",
                                "Coordinate_System/Map_Info"))
in.info.sep <- strsplit(in.info, ",")

ll.x <- as.numeric(in.info.sep[[1]][4])
ll.y <- as.numeric(in.info.sep[[1]][5])

# let's try to make an RGB image. Let's say red is ~650 nm, green is ~550, and
# blue is ~450

# find the red band
red.index <- which(abs(wavelengths - 650) == min(abs(wavelengths - 650)))

red.array <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                    name = "/MLBS/Reflectance/Reflectance_Data",
                    index = list(red.index, 1:1000, 1:1000))

red.matrix <- red.array[1,,]
red.rast <- rast(red.matrix,
                 crs = crs(paste0("epsg:", in.proj)), 
                 extent = c(ll.x, ll.x + 1000, ll.y, ll.y+1000))

# now green
green.index <- which(abs(wavelengths - 550) == min(abs(wavelengths - 550)))

green.array <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                    name = "/MLBS/Reflectance/Reflectance_Data",
                    index = list(green.index, 1:1000, 1:1000))

green.matrix <- green.array[1,,]
green.rast <- rast(green.matrix,
                 crs = crs(paste0("epsg:", in.proj)), 
                 extent = c(ll.x, ll.x + 1000, ll.y, ll.y+1000))

# now blue
blue.index <- which(abs(wavelengths - 450) == min(abs(wavelengths - 450)))

blue.array <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                      name = "/MLBS/Reflectance/Reflectance_Data",
                      index = list(blue.index, 1:1000, 1:1000))

blue.matrix <- blue.array[1,,]
blue.rast <- rast(blue.matrix,
                   crs = crs(paste0("epsg:", in.proj)), 
                   extent = c(ll.x, ll.x + 1000, ll.y, ll.y+1000))

# stack them up!
rgb.stack <- c(red.rast, green.rast, blue.rast)

# and plot! 
plotRGB(rgb.stack)

# now let's make a CIR image

# first we have to get a NIR layer
nir.index <- which(abs(wavelengths - 750) == min(abs(wavelengths - 750)))

nir.array <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                     name = "/MLBS/Reflectance/Reflectance_Data",
                     index = list(nir.index, 1:1000, 1:1000))

nir.matrix <- nir.array[1,,]
nir.rast <- rast(nir.matrix,
                  crs = crs(paste0("epsg:", in.proj)), 
                  extent = c(ll.x, ll.x + 1000, ll.y, ll.y+1000))

# make a NIR stack
nir.stack <- c(nir.rast, red.rast, green.rast)

# and plot!
plotRGB(nir.stack)





