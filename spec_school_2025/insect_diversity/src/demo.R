library(tidyverse)
library(lidR)

plot_6 <- readLAS('spec_school_2025/insect_diversity/data/mlbs_6.las')
lidR::plot(x = plot_6$X,
           y = plot_6$Y,
           z = plot_6$Z)





LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las <- readLAS(LASfile)

plot(las)
plot(las, color = "Intensity")
plot(las, color = "ScanAngleRank", pal = rainbow)

# If outliers break the color range, use the breaks parameter
las$Intensity[150] <- 1000L
plot(las, color = "Intensity")
plot(las, color = "Intensity", breaks = "quantile", nbreaks = 50)

plot(las, color = "Classification")

# This dataset is already tree segmented
plot(las, color = "treeID")
plot(las, color = "treeID", pal = random.colors)


# single file LAScatalog using data provided in lidR
ctg = readLAScatalog(LASfile)
plot(ctg)
plot(ctg, map = T, map.types = "Esri.WorldImagery")

## End(Not run)