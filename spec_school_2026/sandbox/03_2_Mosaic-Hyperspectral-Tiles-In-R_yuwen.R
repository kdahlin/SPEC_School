# Only mosaic the 8 tiles laying in our AOI
# filter tiles X:[541000:543000];Y:[4135000:4138000] 2*4=8 tiles

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
eastings  <- seq(541000, 543000, by = 1000)
northings <- seq(4135000, 4138000, by = 1000)