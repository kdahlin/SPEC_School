####Disclaimer, my version of R is not compatible with canopyLazR

library('lidR')

file <- '/Users/isamarcortes/Downloads/NEON_D07_MLBS_DP1_542000_4136000_classified_point_cloud_colorized.laz'
test <-readLAS(file)
las <- filter_poi(test, Z >= 1100, Z <= 1250)
plot(las, legend = TRUE)

thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- rasterize_canopy(las, 1, pitfree(thr, edg))
plot(chm)
