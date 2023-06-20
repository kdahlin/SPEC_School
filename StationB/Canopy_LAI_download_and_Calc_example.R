# source('~/R/clean.r')
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

# Notes:
# requires a version of wget on your computer.
# hemispheR code below is pulled directly from the example in the repo link here:
# https://gitlab.com/fchianucci/hemispheR

# setwd
wd <- '~/Current Projects/SpecSchool/SPEC_School/StationB/'
setwd(wd)

# data product ID
dpid <- "DP1.10017.001"

# site ID
site <- "MLBS"

# Date
date <- '2022-10'

# Obtain LAI data
LAI <- loadByProduct(dpID = dpid,
                     site = site,
                     startdate = date,
                     check.size = F)

# Obtain overstory hemispherical list of urls
urls <- grep('overstory',LAI$dhp_perimagefile$imageFileUrl, value = T)
# number of files (for the loop)
nfile <- length(urls)

# Create folder with date name
dir.create(date)
# update WD
setwd(date)

# for loop
for (i in seq(nfile)) {
    # subset URL
    iurl <- urls[i] 
    # obtain image name
    name <- unlist(strsplit(iurl, '/'))[10]
    # download file
    download.file(destfile = name, url=iurl, method='wget', quiet = T)
    
    ## ~~ LAI calculation from hemispheR package ~~ ##
    image <- name 
    # set to T for debugging
    display = F
    # import fisheye
    img<-import_fisheye(image,
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
    
    # calculate LAI
    canopy<-canopy_fisheye(gap.frac)
    canopy
    # LAI = 4 using high-res
    # LAI = 3 using jpeg
    
    # remove the downloaded files
    # comment out if you want to keep the photos
    file.remove(name)
    
    # Data frame creation (currently could be better to include geolocation coordinates)
    if (i == 1) {LAI.vals <- canopy$L} else {LAI.vals <- c(LAI.vals, canopy$L)}
    
}

write.csv(LAI.vals, paste0('LAI_values_',date,'.csv'))
