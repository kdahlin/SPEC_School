###############################################################################
# Code for Project EDDIE: Remote Sensing of Plants and Topography in R
# Author: Kyla M. Dahlin
# Last Update: 20191029
###############################################################################

# first install packages (you only need to do this once per computer)
#install.packages(c("rgdal", "raster", "ggplot2", "terra"))

# load the packages you need for this module
library(rgdal)
library(ggplot2)
library(viridisLite)
library(terra)

# set the workding directory (where R will look for your files)
setwd("C:/Users/kdahlin/Dropbox/EDDIE_module/NEON_data_organized/")

# load the data! these paths will need to be changed for other data sets
# DTM = Digital Terrain Model (= DEM)
dtm <- rast("./ca_soap_2018/NEON_D17_SOAP_DP3_298000_4100000_DTM.tif")

# DSM = Digital Surface Model (= LiDAR first returns, topo + vegetation)
dsm <- rast("./ca_soap_2018/NEON_D17_SOAP_DP3_298000_4100000_DSM.tif")

# NDVI = Normalized Difference Vegetation Index
ndvi <- rast("./ca_soap_2018/NEON_D17_SOAP_DP3_298000_4100000_NDVI.tif")

# calculate vegetation height as the DSM minus the DTM
veg.ht <- dsm - dtm

# stack all the data so we can look at it in one plot
all.data <- c(dtm, dsm, ndvi, veg.ht)

# give all the layers names in the 'all.data' stack so the names plot nicely
names(all.data) <- c("DTM", "DSM", "NDVI", "Vegetation.Height")

# plot these four maps
# in PCs (or mac?), x11() opens a new window for plotting
# the plot function here may take up to 30 seconds to plot
x11() 
plot(all.data, 
     col = viridis(255))

# calculate slope and aspect. note here we use degrees for slope, as that is our
# final product, but we use radians for aspect as we are going to calculate 
# 'northness' (cosine of aspect) which will read in radians

# calculate slope
slope <- terrain(dtm, v = 'slope', unit = 'degrees', neighbors = 8)

# calculate aspect
aspect <- terrain(dtm, v = 'aspect', unit = 'radians', neighbors = 8)

# calculate northness
north <- cos(aspect)


# add these new layers to the all.data stack so they can be plotted easily 
# together
all.data$slope <- slope
all.data$north <- north

# plot all six maps! re-size the x11 window so that each plot box is 
# approximately a square then re-run the plot command so it looks nice

x11() # only need this if you closed the previous window
plot(all.data, 
     col = viridis(255))

# save this image as a .png file by going to File > Save As > Png in the x11
# window, then put it into your worksheet

###############################################################################
# 
# PAUSE here for a discussion of hypotheses from this small area what do 
# students expect? will there be a relationship between greenness or vegetation
# height and topography?
#
###############################################################################
######################### END ACTIVITY A ######################################
###############################################################################

# to do statistical analyses of this data it's helpful to convert it from a
# bunch of rasters to a data table (data.frame in R)

# to build a data frame, first make an empty one that has the same number of
# rows as one of the raster layers has cells but no columns yet
df.data <- as.data.frame(matrix(NA, nrow = ncell(dtm), ncol = 0))

# extract each data set from the raster data and put it in a column of the data
# frame. 
df.data$veg.ht <- values(veg.ht)
df.data$ndvi <- values(ndvi)
df.data$dtm <- values(dtm)
df.data$slope <- values(slope)
df.data$north <- values(north)

# because this is a LARGE data set (1 million rows) it can take a long time to
# plot. We are going to take a random 1% subsample (10,000 rows) so that 
# we can plot/analyze data a bit quicker. Given the large size of the data set,
# we expect results to be similar for the subsample as for the whole data set.

# first identify which random samples we want to select
which.sub <- sample(1:nrow(df.data), 
                 size = (nrow(df.data)*0.01))

# use indexing to remove just those 100,000 rows
df.sub <- df.data[which.sub,]

# now let's plot the relationships between the vegetation variables (ndvi & 
# veg height) against the topographic variables (elevation, slope, northness)

# first we have to tell R that we want a multipanel figure using mfrow
par(mfrow = c(2,3), 
    pch = ".", 
    col = rgb(115, 123, 233, 180, maxColorValue = 255))
plot(df.sub$dtm,
     df.sub$ndvi)
plot(df.sub$slope,
     df.sub$ndvi)
plot(df.sub$north,
     df.sub$ndvi)
plot(df.sub$dtm,
     df.sub$veg.ht)
plot(df.sub$slope,
     df.sub$veg.ht)
plot(df.sub$north,
     df.sub$veg.ht)

# look at these plots and make hypotheses about which of these clouds of points
# we think might have a statistical relationship

# now let's actually calculate correlation (Pearson's R) for each of these 
# relationships. Write these results ('cor' and 'p-value') into the handout
# table
cor.test(df.sub$dtm, df.sub$veg.ht)
cor.test(df.data$dtm, df.data$ndvi)
cor.test(df.data$slope, df.data$veg.ht)
cor.test(df.data$slope, df.data$ndvi)
cor.test(df.data$north, df.data$veg.ht)
cor.test(df.data$north, df.data$ndvi)

###############################################################################
# 
# PAUSE here for a discussion of these different relationships. How do these
# numbers match up with what we expected based on the maps and the plots?
# what do we think about the relevance of p-values in large data sets?
#
###############################################################################
######################### END ACTIVITY B ######################################
###############################################################################

# choose one of the relationships we tested previously for the whole class to
# look at across the US at NEON sites. Assign or have each student choose a
# location to analyze, then go back through the code to calculate the 
# correlation and contribute to the map of sites in the powerpoint.

