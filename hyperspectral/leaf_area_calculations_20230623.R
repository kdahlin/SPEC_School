# load the terra package to deal with raster images
library(terra)

# read in leaf area scans at 150 dpi and calculating area

setwd("D:/Users/kdahlin/Dropbox/NSF_CAREER_2021/SPEC_School/2023/field_data/")


# list all the leaf scan jpg files in the directory
leaf.files <- list.files("leaf_scans/", 
                        pattern = c("\\.jpg$"),
                        full.names = TRUE)

# make a data frame to write names to
out.data <- as.data.frame(matrix(NA, nrow = length(leaf.files), ncol = 4))
names(out.data) <- c("ID", "pixels", "area_cm2", "area_m2")
out.data$ID[1:9] <- paste0("MLBS_00", 1:9)
out.data$ID[10:length(leaf.files)] <- paste0("MLBS_0", 10:length(leaf.files))

# loop to read in each file and calculate area in cm2 and m2
for (i in 1:length(leaf.files)) {
  in.pic <- rast(leaf.files[i])
  pix.count <- as.numeric(global(in.pic[[1]] == 0, fun = "sum"))
  out.data$pixels[i] <- pix.count
  out.data$area_cm2[i] <- pix.count * ((2.53^2)/(150^2))
  out.data$area_m2[i] <- out.data$area_cm2[i] / (100^2)
}

# write csv
write.csv(out.data, "MLBS2023_SPEC_School_leaf_area.csv",
          row.names = TRUE)
