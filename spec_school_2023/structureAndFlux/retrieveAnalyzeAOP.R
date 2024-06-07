## AOP stuff
library(sp) 
library(sf)
library(raster)
library(neonUtilities)
library(neonOS)
library(geoNEON)

library(RcppRoll)
library(lubridate)
library(dplyr)


options(stringsAsFactors=F)
#set working directory
wd <- "~/code/spec_school/data"
setwd(wd)
yr <- '2018'
site_oi <- 'MLBS'
dproduct <- "DP3.30026.001"#"DP3.30014.001"

footprint_info <- st_read('~/code/spec_school/data/footprintData/90percent_flux.shp')

test <- footprint_info[(footprint_info$SiteID == site_oi) & (footprint$Type == "major"),]

buffersize <-unique(test$Distance)

byTileAOP(dpID=dproduct, 
          site=site_oi, 
          year=yr,
          easting ="542071.17",
          northing="4136945.68",
          buffer=buffersize,
          savepath="~/code/spec_school/data/MLBS")

# vegindwd

viwd <- '~/code/spec_school/data/MLBS/DP3.30026.001/neon-aop-products/2018/FullSite/D07/2018_MLBS_3/L3/Spectrometer/VegIndices/'
# AOP working directory
tmpwd2015 <- '~/code/spec_school/data/MLBS/DP3.30014.001/neon-aop-products/2015/FullSite/D07/2015_MLBS_1/L3/Spectrometer/FPAR/'

tmpwd2017 <- '~/code/spec_school/data/MLBS/DP3.30014.001/neon-aop-products/2017/FullSite/D07/2017_MLBS_2/L3/Spectrometer/FPAR/'

tmpwd2018 <- "~/code/spec_school/data/MLBS/DP3.30014.001/neon-aop-products/2018/FullSite/D07/2018_MLBS_3/L3/Spectrometer/FPAR/"

tmpwd2021 <- '~/code/spec_school/data/MLBS/DP3.30014.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L3/Spectrometer/FPAR/'


rastlist <- list.files(path=tmpwd2017, pattern='fPAR.tif$', 
                                   all.files=TRUE, full.names=TRUE)


allrasters <- lapply(rastlist, raster)

fpar <- do.call(merge, allrasters)

#fpar <- raster("~/code/spec_school/data/MLBS/DP3.30014.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L3/Spectrometer/FPAR/NEON_D07_MLBS_DP3_542000_4136000_fPAR.tif")

###switch to your working directory 
# ndvi <-raster("/Users/jnavarro/Desktop/SPEC_School_2023/DP3.30026.001/neon-aop-products/2021/FullSite/D07/2021_MLBS_4/L3/Spectrometer/VegIndices/NEON_D07_MLBS_DP3_542000_4136000_VegetationIndices/NEON_D07_MLBS_DP3_542000_4136000_SAVI.tif")
plot(fpar, main='MLBS, 2021')

footprints <- st_read("~/code/spec_school/data/footprintData/basic_footprint.shp")
footprint_ <- footprints[footprints$sitename==site_oi,]
footprint_ <- st_transform(mlbs_footprint, crs=st_crs(fpar))

plot(footprint_, add=TRUE)
points(x= c(542071.17), y=c(4136945.68), add=TRUE)

plot(raster::mask(fpar, footprint_), add=FALSE)

fpar_vals <- unlist(extract(fpar, mlbs_footprint))

hist(fpar_vals)

mean(fpar_vals, na.rm=TRUE)
sd(fpar_vals, na.rm=TRUE)
