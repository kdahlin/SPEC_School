# install.packages("neonUtilities")
library(neonUtilities)

# Define parameters
product_code <- "DP1.30010.001"
product_code <- 'DP3.30010.001'
site <- "RMNP"
year <- 2020

out_file_path <- '/Volumes/rs-016/ersamlab/hyperspec_id_group'

# byTileAOP(): Downloads remote sensing data for the specified data product, subset to tiles that intersect a list of coordinates.

zipsByProduct(dpID = product_code,
              site = site,
              startdate = paste0(year, "-01"),
              enddate = paste0(year, "-12"),
              package = "basic",
              savepath = out_file_path,
              check.size = TRUE)

byTileAOP(dpID = product_code,
          site = site,
          year =  year,
        
          savepath = out_file_path,
          check.size = TRUE)

# Download the data
loadByProduct(dpID = product_code,
                      site = site,
                      startdate = paste0(year, "-01"),
                      enddate = paste0(year, "-12"),
                      package = "basic",
                      savepath = out_file_path,
                      check.size = TRUE)

