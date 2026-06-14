# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------
# set this up to be read in for the other scripts

# Define the path to external (not in rproj) data storage 
# (HPCC or whatever your data is located)
root <- "X:" 

# Site Domain, Code and Year (following NEON format)
domain <- "D01"
site <- "BART" 
year <- "2025"  

# define EPSG code of your spatial data UTM zone
epsg <- 32619

# working date - for new folders/files generated, they will have this date added
# can be a YYYYMM or YYYYMMDD but keeping something will let future users know
# when the data was processed
wd <- "20260602"