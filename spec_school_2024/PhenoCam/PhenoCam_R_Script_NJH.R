###############################################################################
###############################################################################
## 
## 04/23/2021
##
## Written by: Nautica Jones Harriott (nautica00jones@gmail.com) & Meghan Blumstein (blumsteinm@gmail.com) 
##
## Objective: To extract GCC data and bud burst dates from individual red oaks (Q. rubra) in phenocam images
##
## Inputs: Phenocam images from select sites (https://phenocam.sr.unh.edu/webcam/gallery/)
##      
## Outputs: GCC curves and key phenological dates for individual tress in the analysis. (csv file format)
##
###############################################################################
###############################################################################

rm(list = ls())

###############################################################################
##                              SETUP
###############################################################################

## Set my working directory to the home folder for all of my phenocam images
## and eventual ROIs
# wd <- "/n/academic_homes/g_81346/u_410067_g_81346"
#wd <- "/Users/meghs/Dropbox/Mentoring/REU_2021/Data/"
#setwd(wd)

## Load Packages
library(raster)     ## package enables processing of spatial raster files
library(phenopix)   ## package used to analyze phenology data from phenocams
library(doParallel) ## Allows for parallelization of functions
library(parallel)   ## Sets up clusters for parallelization
library(data.table)
library(phenocamapi) ## additional package for working with phenocam images and metadata
#if(!require(devtools)) install.packages('devtools')
#devtools::install_github('bnasr/phenocamapi')

library(plyr)
library(zoo)
library(rgdal)
## Load updated extract VIs function
source(file = "C:/Users/nauti/Documents/extractVIs_MB_v2.R")

## Set up parallel processing
#ncores <- 6
#cl <- makeCluster(ncores, type = "FORK")
#registerDoParallel(cl)

###############################################################################
##                              USER INPUT
###############################################################################

## Set up variables that will change on each run through
img_nme   <- "Green_Ridge_MD.jpg"
img_path  <- "gr/gr_img/"
roi_path  <- "gr/gr_roi/"
vis_path  <- "gr/gr_vi/"
roi_nmes  <- paste0("gr_", 1:4)
nroi      <- 4
file_type <- ".jpg"

## List of our site names from phenocam
sites    <- c("greenridge1","umichbiological", "umichbiological2", "uwmfieldsta", "thompsonfarm2N")

## Site Name for this iteration
site_run <- sites[1] ## Which site are we currently evaluating
outfile   <- paste0("gr/", site_run, "_Season_Dates.csv")

###############################################################################
##                         GET ROIs for All Sites
## This section of code sets up the variables for each site run and enables
## the user to draw ROIs of interest
###############################################################################

## Load in my first phenocam image
img <- brick(paste0(img_path, img_nme)) ## paste puts the two strings together into the full path

## Plot it to check it loaded correctly
plotRGB(img)

## Extract the date information from the image
## This is just a check to see if
img_date <- extractDateFilename(paste0(img_path, img_nme), "yyyy_mm_dd_HHMM")

# # Draw ROIs
DrawMULTIROI(path_img_ref = paste0(img_path, img_nme),
             nroi = nroi,
             path_ROIs = roi_path,
             roi.names = roi_nmes,
             file.type = file_type)

###############################################################################
##                   ACCESS ALL SITE IMAGES
## This stretch of code will pull all noon-time images associated with a site
## into the images folder 
###############################################################################

## Get phenocam metadata for these sites
metadata <- get_phenos()
metadata <- metadata[which( metadata$site %in% sites ),] ## take only the rows which have site names found in the list "sites"

## Figure out the start and end date of the time series for each site via the metadata
## (The row that is equal to the site of interest and the columns that have the first and
## last date of image collection)
start_end  <- metadata[ which(metadata$site == sites[1]), c("date_first", "date_last")]
start_year <- year(start_end[[1]])
end_year   <- year(start_end[[2]])
yois       <- start_year:end_year ## years of interest

## Create a temporary directory in your working directory to store the images
## this will be deleted (along with the images) at the end of analysis to free
## up memory for the next site
# tmp_dir <- tempdir()
tmp_dir <- img_path

## Download all images for the site of interest
## This only works one year at a time, so we must loop 
## through all of the years to get all of the images of interest
for(y in yois){
  
  print(y)
  
  download_midday_images(site = site_run,
                         y = y,
                         months = 1:12,## says we want all months of data
                         days = 1:31, ## and all days
                         download_dir = tmp_dir)
  
}


###############################################################################
##                   EXTRACT VIS FOR ALL IMAGES
## here we get the gcc curves over the images of interest
###############################################################################

## Extract the vegetation indicies for this image
VIs <- extractVIs_MB(img.path = tmp_dir, ## Change to the temporary directory where we downloaded images to 
                     roi.path = roi_path, 
                     vi.path  = vis_path,
                     date.code = "yyyy_mm_dd_HHMM", 
                     file.type = ".jpg", 
                     ncores = ncores,
                     plot = F)


###############################################################################
##                  shut off Cores and Clean out Files
## DO NOT RUN UNTIL YOU ARE HAPPY WITH DATA - CHECK PNG PLOTS TO ENSURE
## THINGS LOOK OK 
###############################################################################

## Stop Clusters
#stopCluster(cl)

## Deletes the temporary directory with images
# unlink(tmp_dir)


###############################################################################
##                        FIT CURVES TO DATA 
###############################################################################

## Get the data file name created by the extract VIs data 
## the data name is "VI.data"
vi.file.nme <- list.files(vis_path, pattern = "VI.data.Rdata")
load(paste0(vis_path, vi.file.nme))

## Examine VI.data
## it should be a list, with each entry representing a different ROI
class(VI.data)
rois <- names(VI.data)

## Change plotting parameters (change margins and distance of text to plot)
par(mar = c(3.5, 3.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))


## GIANT LOOP
## The outer loop goes through each ROI in the dataset
## The inner loop goes through each year of data and fits a curve, then 
## estimates the start and end of the growing season from that curve
## The end result is a table for each ROI and each year that indicates
## the start and end of the growing season for that ROI/year combo
roi_df <- c()
for(r in rois){
  
  print(paste("Now running...", r))
  
  ## Select the data for current roi
  VI    <- VI.data[[r]]
  
  ## First, we must filter the data. From the vignette: https://www.researchgate.net/publication/289374477_phenopix_R_package_vignettes_13_base_vignette
  ## "Data retrieved from images often need robust methods for polishing the time
  ## series. Bad weather conditions, low illumination, dirty lenses are among the
  ## most common issues that determine noise in the time series of vegetation indexes"
  VI.Filtered <- autoFilter(VI, filter = c("spline", "max"), plot = T)
  
  ## !! Remove the roi drawing image by date if included !!
  # VI.Filtered <- VI.Filtered[index(VI.Filtered) != "2021-05-04",]
  
  ## Get list of years in the data
  yr.vector <- as.numeric(format(index(VI.Filtered), "%Y"))
  years     <- unique(yr.vector)
  
  ## Loop over each year of data and fit the curve
  output_df <- c() ## set up empty dataframe to fill with start and end of season values
  for(y in years){
    
    ## Print out what year is currently running
    print(paste("     ...", y))
    
    ## Subset data to just one year of growth
    VI.Filtered.yr <- VI.Filtered[which(yr.vector == y), "gcc"]
    
    ## Skip over years where there are only a few observations or 
    ## observations begin after the start of spring
    doy.vector <- as.numeric(format(index(VI.Filtered.yr), "%j"))
    if(doy.vector[1] > 150){next} ## Skip years where phenocam is installed over summer
    if(doy.vector[length(doy.vector)] < 200){next}## skip incomplete years 
    
    ## Replace the date vector with julian days
    index(VI.Filtered.yr) <- doy.vector
    fit <- FitDoubleLogGu(VI.Filtered.yr)
    
    ## Plot lines on original GCC plot to indicate it works 
    lines(fit$predicted ~ index(VI.Filtered[which(yr.vector == y)]), 
          col = "steelblue",lwd = 5)
    
    ## Get out parameters
    sos <- fit$params[["t01"]]
    eos <- fit$params[["t02"]]
    
    ## Store output parameters in a dataframe
    output_df <- rbind(output_df, c(r, y, sos, eos))
    
  }
  
  ## Add data to ROI dataframe
  roi_df <- rbind(roi_df, data.frame(Site = site_run, output_df))
  
}

## Convert from matrix to dataframe
roi_df <- as.data.frame(roi_df)

## Add column Names
colnames(roi_df) <- c("Site", "ROI", "Year", "Start of Season", "End of Season")

## Save out file
write.csv(roi_df, outfile)








