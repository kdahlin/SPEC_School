# This script was developed to mosaic all Hyperspectral tiles into one Raster.

# library(hsdar)

## ----load-libraries, results="hide"-----------------------------------------------------------------------------------------------------------------------------
library(terra)
library(rhdf5)
library(neonUtilities)

# # parallelize
# library(future.apply)
# plan(multisession, workers = 6)

## set input data directory - note this is written to work with a mapped network
## drive, you need to change "X:" to "/mnt/research/ersamlab/" if you are working
## via OnDemand
data.dir <- file.path("X:", "shared_data", "NEON_AOP_data", "MLBS", "2023",
                      "L3", "Spectrometer", "Reflectance")

## set your save data directory - same note as above re: OnDemand
save.dir <- file.path("X:", "shared_data", "NEON_proc_data", "MLBS", "2023")

# Step 1: Build all filenames automatically
eastings  <- seq(539000, 544000, by = 1000)
northings <- seq(4134000, 4138000, by = 1000)

tiles <- expand.grid(
  easting  = eastings,
  northing = northings
)

h5_files <- file.path(
  data.dir,
  paste0(
    "NEON_D07_MLBS_DP3_",
    tiles$easting, "_",
    tiles$northing,
    "_bidirectional_reflectance.h5"
  )
)

# Step 2: Check files exist
file.exists(h5_files)
sum(file.exists(h5_files)) #30

# get the Reflectance_Data attributes
# h5_file <- paste0(data.dir,"/NEON_D07_MLBS_DP3_541000_4137000_bidirectional_reflectance.h5")
h5_file = h5_files[1]
reflInfo <- h5readAttributes(h5_file,"/MLBS/Reflectance/Reflectance_Data")

h5NoDataValue <- as.integer(reflInfo$Data_Ignore_Value)
cat('No Data Value:',h5NoDataValue)

# sprc(h5_files)
# sprc(h5_file)

# Step 2: Function to read a 6-band tile
# create a list of the bands (R,G,B) we want to include in our stack
# B = 19
# G = 34;35
# R = 58;53
# NIR = 95
# SWIR1 = 253
# SWIR2 = 350

read_tile_6band <- function(h5_file, bands = c(19,34,58,95,253,350)) {
  
  # CRS
  epsg <- h5read(
    h5_file,
    "/MLBS/Reflectance/Metadata/Coordinate_System/EPSG Code"
  )
  
  # Spatial metadata
  reflInfo <- h5readAttributes(
    h5_file,
    "/MLBS/Reflectance/Reflectance_Data"
  )
  
  ext_tile <- ext(
    reflInfo$Spatial_Extent_meters[1],
    reflInfo$Spatial_Extent_meters[2],
    reflInfo$Spatial_Extent_meters[3],
    reflInfo$Spatial_Extent_meters[4]
  )
  
  noData <- as.integer(
    reflInfo$Data_Ignore_Value
  )
  
  band_rasters <- lapply(bands, function(b){
    
    x <- h5read(
      h5_file,
      "/MLBS/Reflectance/Reflectance_Data",
      index = list(b,NULL,NULL)
    )
    
    x <- t(x[1,,])
    
    x[x == noData] <- NA
    
    r <- rast(x)
    
    ext(r) <- ext_tile
    
    crs(r) <- paste0("EPSG:", epsg)
    
    r
  })
  
  s <- rast(band_rasters)
  
  names(s) <- c(
    "Blue",
    "Green",
    "Red",
    "NIR",
    "SWIR1",
    "SWIR2"
  )
  
  return(s)
}

# Step 3: Create 6-band GeoTIFF for each tile
tile_tifs <- character(length(h5_files))

for(i in seq_along(h5_files)){
  
  cat(i, "of", length(h5_files), "\n")
  
  r <- read_tile_6band(h5_files[i])
  
  outfile <- file.path(
    save.dir,
    paste0("mosaic/",
      tools::file_path_sans_ext(
        substr(basename(h5_files[i]), start = 1, stop = 32)
      ),
      "_6band.tif"
    )
  )
  
  writeRaster(
    r,
    outfile,
    overwrite = TRUE
  )
  
  tile_tifs[i] <- outfile
}

# Step 4: Mosaic all 30 tiles
rasters <- lapply(tile_tifs, rast)

# mosaic should do better job than merge
# mosaic_6band <- do.call(
#   merge,
#   rasters
# )

mosaic_6band_v2 <- do.call(mosaic, c(rasters, fun="median")) #or fun="median"

# improved version
r <- sprc(lapply(h5_files, read_tile_6band))
mosaic_6band <- mosaic(r)


mosaic_6band
mosaic_6band_scaled <- mosaic_6band/as.integer(reflInfo$Scale_Factor)

# this version of mosaic was bad
# proper reasons can be: handle a few critical radiometric and geometric steps
# (1) inconsistent radiometry between scenes, 
# (2) imperfect cloud/shadow masking, 
# (3) simple compositing instead of quality-based blending
# outfile <- file.path(
#   save.dir,
#   paste0("mosaic/",
#          tools::file_path_sans_ext(
#            substr(basename(h5_files[1]), start = 1, stop = 32)
#          ),
#          "_6band.tif"
#   ))

plotRGB(
  # mosaic_6band,
  # mosaic_6band_v2,
  mosaic,
  r=3,g=2,b=1,
  stretch = "lin")

plotRGB(mosaic_6band_scaled,
        r=3,g=2,b=1,
        stretch = "lin")

# Step 5: Save final mosaic
writeRaster(
  # mosaic_6band,
  mosaic_6band_scaled,
  file.path(
    save.dir, "mosaic",
    "MLBS_2023_6band_mosaic.tif"
  ),
  overwrite = TRUE
)


ra_file <- file.path(save.dir, "mosaic","MLBS_2023_6band_mosaic.tif")
mosaic <- rast(ra_file)
