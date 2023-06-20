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


wd <- '~/Current Projects/SpecSchool/NEON_LAI/'
setwd(wd)

dpid <- "DP1.10017.001"
site <- "MLBS"
date <- '2022-10'

LAI <- loadByProduct(dpID = dpid,
                     site = site,
                     startdate = date,
                     check.size = F)

urls <- grep('overstory',LAI$dhp_perimagefile$imageFileUrl, value = T)
nfile <- length(urls)
dir.create(date)
setwd(date)

for (i in seq(nfile)) {
    iurl <- urls[i]
    name <- unlist(strsplit(iurl, '/'))[10]
    
    download.file(destfile = name, url=iurl, method='wget', quiet = T)
    
    # LAI calculation from hemispheR package 
    
    image <- name 
    display = F
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
    
    canopy<-canopy_fisheye(gap.frac)
    canopy
    # LAI = 4 using high-res
    # LAI = 3 using jpeg
    
    # Data frame creation
    if (i == 1) {LAI.vals <- canopy$L} else {LAI.vals <- c(LAI.vals, canopy$L)}
    
}
