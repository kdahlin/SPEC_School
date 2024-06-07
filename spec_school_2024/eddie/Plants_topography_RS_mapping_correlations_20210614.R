###############################################################################
# Code for Project EDDIE: Remote Sensing of Plants and Topography in R 
# Activty C - mapping the correlations
# Author: Kyla M. Dahlin
# Last Update: 20210614
###############################################################################
# to make maps we'll rely on an unusual package (may later ask you to install
# 'rnaturalearthdata' too)
install.packages("rnaturalearthhires", 
                 repos = "http://packages.ropensci.org", 
                 type = "source")

# load libraries for making maps (install these too if you haven't already)
library(ggplot2)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)

# set your working directory to where your table of neon site info is
# stored
setwd("***/NEON_data_organized/")

# read in the NEON point data (with your R2 values added in the R2 column)
neon.points <- read.csv("neon_site_info_all_2020.csv",
                        stringsAsFactors = FALSE)

# if you've entered the R2 values for your students' analysis, subset the
# data 
neon.points.sub <- subset(neon.points,
                          !is.na(neon.points$cor.R))

# if you want to plot the whole extent of the NEON sites just do this
# note that because some of the sites are very close to eachother, this can
# look a bit messy
#neon.points.sub <- neon.points

# map title
m.title <- "NDVI x Topography Correlations at NEON sites"
m.subtitle <- "created on March 20, 2020, by K. Dahlin"

# calculate the extent of the map based on the latitude and longitude in the
# data table
x.min <- min(neon.points.sub$Longitude) - 5
x.max <- max(neon.points.sub$Longitude) + 5
y.min <- min(neon.points.sub$Latitude) - 5
y.max <- max(neon.points.sub$Latitude) + 5

world <- ne_countries(scale = "medium", returnclass = "sf")
us.states <- ne_states(country = "United States of America", 
                       returnclass = "sf")

# set the font sizes for the site IDs and correlation values
site.size <- 2
cor.size <- 4


theme_set(theme_bw())

x11() # opens a new plotting window
# plot the map - note, you can change any/all of these parameters as you choose
# some that will change depending on your extent are 'nudge_y' 
# making a plot to plot points in R
# info from here: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
ggplot(data = world) +
  geom_sf(fill = "darkseagreen2") +
  geom_sf(data = us.states, 
          fill = NA, 
          color = "white") +
  geom_sf(color = "black",
          fill = NA) +
  coord_sf(xlim = c(x.min, x.max),
           ylim = c(y.min, y.max),
           expand = FALSE) +
  geom_text(data = neon.points.sub, 
            aes(x = Longitude, y = Latitude, label = Site.ID),
                color = "darkblue", size = site.size) +
  geom_text(data = neon.points.sub, 
            aes(x = Longitude, y = Latitude, label = cor.R),
            color = "darkred", size = cor.size,
            nudge_y = 0.4) + 
  ggtitle(m.title,
          subtitle = m.subtitle)



