# NOT READY FOR PRIME TIME!
# Title: 00_organize_leaf_data.R
# Date: 02/20/2026
# Authors:  KMD
# This script is for reading in leaf trait data from excel spreadsheets and 
# checking that values are sensible

# Load libraries
library(readxl)
library(plyr)

#-------------------------
# USER DEFINED VARIABLES
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# date of "Individual Files" folder you want to use
indiv.date <- "20260522"

# set the input for the field data files 
data.loc <- file.path(root, "shared_data", "NEON_field_data",
                       site, year, "ERSAM", "Field_Data", "Tabular_Data")

in.points <- read_xlsx(paste0(data.loc, "FieldData_UMBS_20190815.xlsx"))
in.points$`Sample Height (m)` <- as.numeric(in.points$`Sample Height (m)`)
in.points$`DBH (cm)` <- as.numeric(in.points$`DBH (cm)`)

in.leaf <- read_xlsx(paste0(data.loc, "UMBS_LabData_20190815_v2.xlsx"))

########## merge the two data sets from original off hpcc ######################
in.field <- merge(in.points,in.leaf,
                  by = "ID",
                  all.x = TRUE)

# remove duplicate date column
in.field <- in.field[,-7]

names(in.field)[2] <- "Date"

# write this clean table
write.csv(in.field, "UMBS2019_clean_plant_info_20240123.csv")

####### start here to not re-merge original data from hpcc #########
in.field <- read.csv("UMBS2019_clean_plant_info_20240123.csv")

# rename so names aren't so clunky for analysis
names(in.field) <- c("num", "ID", "date", "species", "can_pos", "sample_ht_m", 
                     "dbh_cm", "wet_wt_g", "dry_wt_g", "leaf_area_cm2", "sla_m2g",
                     "lma_gm2", "perc_N", "perc_C")

# calculate water fraction 
in.field$water_frac <- (in.field$wet_wt_g - in.field$dry_wt_g) / in.field$wet_wt_g

# note that there are some HIGH water content leaves. Mostly the understory
# ferns but one Betula that may need to be discarded from future analysis

in.field[which(in.field$water_frac > 0.7),]
in.field <- in.field[-94,]

unique(in.field$species)

field.summary <- ddply(in.field, "species", summarise,
                       count.n = length(species),
                       lma.mean = mean(lma_gm2),
                       lma.sd = sd(lma_gm2),
                       water.frac.mean = mean(water_frac),
                       water.frac.sd = sd(water_frac),
                       perc_N.mean = mean(perc_N),
                       perc_N.sd = sd(perc_N),
                       perc_C.mean = mean(perc_C),
                       perc_C.sd = sd(perc_C))

write.csv(field.summary, "UMBS2019_summarized_field_data_20210416.csv",
          row.names = FALSE)






