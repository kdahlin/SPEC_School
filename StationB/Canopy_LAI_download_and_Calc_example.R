source('~/R/clean.r')
library(sp)
library(raster)
library(neonUtilities)
library(neonOS)
library(geoNEON)
# devtools::install_github('NEONScience/NEON-geolocation/geoNEON')
library(geoNEON)
options(stringsAsFactors=F)
# install.packages('hemispheR')
library(hemispheR)
library(dplyr)
library(ggplot2)


wd <- '/Users/gracemcleod/Documents/SPEC/In_person_week/SPEC_School/StationB'
setwd(wd)


# DOWNLOAD LAI DATA ........................................................................................

dpid <- "DP1.10017.001"
site <- "MLBS"
startdate <- '2022-05'
enddate <- "2022-06"

LAI <- loadByProduct(dpID = dpid,
                     site = site,
                     startdate = startdate,
                     enddate= enddate,
                     check.size = F)

urls <- grep('overstory',LAI$dhp_perimagefile$imageFileUrl, value = T)
nfile <- length(urls)
dir.create(startdate)
setwd(startdate)

for (i in seq(nfile)) {
    iurl <- urls[i]
    name <- unlist(strsplit(iurl, '/'))[10]
    
    download.file(destfile = name, url=iurl, quiet = T)
    
    # LAI calculation from hemispheR package 
    
    image <- name 
    display = F
    img<-import_fisheye(iurl, 
                        channel = 'B',
                        circ.mask=list(xc=80,yc=60,rc=80), #xcenter, ycenter, radius
                        circular=F,
                        gamma=1,
                        stretch=T,
                        display=display,
                        message=T)
    
    
    # Once imported, the function performs image classification using a single automated thresholding 
    # from the auto_thresh() functionality of the autothresholdr package:
    img.bw<-binarize_fisheye(img,
                             method='Otsu',
                             zonal=F,
                             manual=NULL,
                             display=display,
                             export=F)
    
    
    # The gapfrac_fisheye() function retrieve the angular distribution from classified fisheye images, 
    # considering the fisheye lens correction, as:
    gap.frac<-gapfrac_fisheye(
        img.bw,
        maxVZA = 90,
        lens = "equidistant",
        startVZA = 0,
        endVZA = 80,
        nrings = 7,
        nseg = 8,
        display=display,
        message = F
    )
    
    canopy<-canopy_fisheye(gap.frac)
    canopy
    # LAI = 4 using high-res
    # LAI = 3 using jpeg
    
    # Data frame creation
    if (i == 1) {LAI.vals <- canopy$L} else {LAI.vals <- c(LAI.vals, canopy$L)}
    
}


# INCORPORATE IMAGE AND PLOT DATA ........................................................................................

# Make a master dataframe
LAI_df <- as.data.frame(LAI.vals) # calculated LAI values
LAI_df$imageFileUrl <- urls[1:113] # I stopped the loop early and don't have all of the image files.

# test
image_plot <- joinTableNEON(LAI$dhp_perimagefile,
                      LAI$dhp_perplot,
                      name1="dhp_perimagefile",
                      name2="dhp_perplot")
# subset to just the images I downloaded
sub_imageplot <- subset(image_plot, imageFileUrl %in% LAI_df$imageFileUrl)
# merge with LAI_df
LAI_df <- merge(LAI_df, sub_imageplot, by="imageFileUrl")


# CHECK OUT THE DATA! ........................................................................................

ggplot(LAI_df, aes(x=startDate.y, y=LAI.vals, color=plotID)) +
  geom_smooth() +
  theme_bw()






