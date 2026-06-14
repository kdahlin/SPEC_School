# This script is for converting spectrometer measurements into a data frame 
# Author: TRG, KMD
# Date: 05/27/2026

# load necessary packages 
library(spectrolab)

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# -----------------------------------
# MAIN WORK - turn .sig files into a data frame
# -----------------------------------

# set file path for reading spectrometer files 
dir_path <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM", 
                      "Lab_Data", "Spectra_Data")

# set out file path for saving output file 
out.path <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("3-001_LeafSpectra_", wd))

# make this directory
dir.create(out_path)

# set the out file name
out.file.name <- paste0(site, "_", year, "_leafspectra.csv")

# Read spectra from .sig files inside the folder in the specified path 
in.spectra <- read_spectra(path = dir_path)

# Make a matrix from a `spectra` object
spectra.df <- as.data.frame(in.spectra, fix_names = "none", metadata = TRUE)

# turn file names into sample and spectra numbers
spectra.df$ID <- substr(spectra.df$sample_name, 1, 9)
spectra.df$number <- as.numeric(substr(spectra.df$sample_name, 11,11))

# plot spectra to see if there are any red flags
x11()
matplot(as.numeric(names(spectra.df)[2:(ncol(spectra.df)-2)]), 
        t(spectra.df[2:(ncol(spectra.df)-2)]), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Lab Leaf Reflectance",
        main = paste("Lab Spectra for", site, year))

# write csv
write.csv(spectra.df, file.path(out.path, out.file.name), row.names = TRUE)
