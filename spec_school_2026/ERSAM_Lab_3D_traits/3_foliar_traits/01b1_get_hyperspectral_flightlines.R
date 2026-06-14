# NOT READY FOR PRIME TIME!!!







# ------------------------------------------------------------------------------
# TRASH OLD SCRIPT FROM KYLA #
# ------------------------------------------------------------------------------

### reading in point shapefiles and figuring out which flight lines they're in ###

library(rhdf5)
library(raster)
library(rgdal)

# this code will produce a csv with a row for each point x flightline combo
# where do you want to save that info?
out.dir <- "C:/Users/kdahlin/Dropbox/NEON_hsi_lidar/hsi_lidar_ms1/data/"
today <- format(Sys.Date(), "%Y%m%d")
loc <- "HARV"

# read in the points file (shapefile)
in.points <- readOGR(dsn = paste0("X:/shared_data/NEON_proc_data/", 
                                  loc, 
                                  "/2017/kyla_2018_processing"), 
                     layer = "HARV_foliar_traits_FINAL2_2017_20181120", 
                     stringsAsFactors = FALSE)

# check that it's in the projection you want
proj4string(in.points)

# write the coordinates to a data frame with unique ids
in.points.locs <- cbind(in.points$pointIDs, as.data.frame(in.points@coords))

# get a list of files in the directory you're intested in
file.list <- list.files(paste0("X:/shared_data/NEON_AOP_data/", loc, "/2017/HSI"))

# removing non hdf5 files by hand because I'm lazy right now
print(file.list)
#file.list <- file.list[-c(1,2)]

# only pulling out two flightlines that have points for HARV
file.list <- file.list[c(14,15)]
file.count <- length(file.list)

file.loc <- paste0("X:/shared_data/NEON_AOP_data/", loc, "/2017/HSI/")

# now make a table of all of the flightlines you're interested in and their extents
flight.ext <- as.data.frame(matrix(data = NA, nrow = file.count, ncol = 5))
names(flight.ext) <- c("filename", "xmin", "xmax", "ymin", "ymax")

for (i in 1:file.count) {
  # get the coordinate system
  coords <- h5read(file = paste0(file.loc, file.list[i]),
                   name = paste0(loc, "/Reflectance/Metadata/Coordinate_System"))
  
  map.info <- strsplit(coords$Map_Info, split = ",", fixed = T)
  easting.ul <- as.numeric(map.info[[1]][4])
  northing.ul <- as.numeric(map.info[[1]][5])
  
  # get a single reflectance band just for dimensions
  refl.sub <- h5read(file = paste0(file.loc, file.list[i]),
                     name = paste0(loc, "/Reflectance/Reflectance_Data"),
                     index = list(80, NULL, NULL))
  
  refl.stack <- raster(as.matrix(refl.sub[1,,]))
  refl.stack.t <- t(refl.stack)
  
  line.dim <- dim(refl.stack.t)
  y.dim <- line.dim[1]
  x.dim <- line.dim[2]
  
  flight.ext[i,1] <- file.list[i]
  flight.ext[i,2] <- easting.ul
  flight.ext[i,3] <- easting.ul+x.dim
  flight.ext[i,4] <- northing.ul-y.dim
  flight.ext[i,5] <- northing.ul
  
  print(paste("done with ", i, date()))
}

# now make a new dataframe that says which points are in which flight lines
# note this is slightly complicated because points can be in more than one 
# flight line

pointsXflights <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 5))


for (i in 1:dim(in.points.locs)[1]) {
  x.in <- in.points.locs[i,2] <= flight.ext$xmax & in.points.locs[i,2] >= flight.ext$xmin
  y.in <- in.points.locs[i,3] <= flight.ext$ymax & in.points.locs[i,3] >= flight.ext$ymin
  all.in <- x.in & y.in
  files.in <- subset(flight.ext$filename, all.in)
  n <- length(files.in)
  out.info <- as.data.frame(cbind(rep(as.character(in.points.locs[i,1]),n), 
                                  rep(in.points.locs[i,2],n), 
                                  rep(in.points.locs[i,3],n), 
                                  files.in,
                                  rep(n,n)))
  pointsXflights <- rbind(pointsXflights, out.info)
  print(i)
}

names(pointsXflights) <- c("pointIDs", "easting", "northing", "flightline", "line.ct")
write.csv(pointsXflights, paste0(out.dir, loc, "_pointsXflights_", today, ".csv"),
          row.names = FALSE)





