install.packages("neonUtilities")
library(neonUtilities)
library(dplyr)
library(data.table)
library(phenocamapi)
library(lubridate)
library(jpeg)

setwd("E:\\Prof. Qiu Lab\\2024SPECschool\\SPEC_School\\spec_school_2024\\Phenology_climate_project")

#####test for pull, commitxxxxx
####Hanshi will process the PhenoCam data (time series: 1 day)
####The time covers 2018-2022
##obtain the phenoCam data of MLBS (understory and overstory)
# obtaining the phenocam site metadata from the server as data.table
phenos <- get_phenos()
# to obtain the UN 1000 from MLBS
MLBS_under_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = 'roistats')
View(MLBS_under_UN_1000)
MLBS_over_DB_2000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00033', vegType = 'DB', roiID = 2000, type = 'roistats')
View(MLBS_over_DB_2000)

##Date cleaning
# Remove columns that contain only NA values
MLBS_under_UN_1000 <- MLBS_under_UN_1000 %>%
  select_if(~ !all(is.na(.)))

# Define the column names
colnames(MLBS_under_UN_1000) <-   c("date", "local_std_time", "doy", "filename",
                                                "solar_elev", "exposure", "awbflag", "mask_index",
                                                "gcc", "rcc", "r_mean",	"r_std",
                                                "r_5_qtl","r_10_qtl",	"r_25_qtl",	"r_50_qtl",
                                                "r_75_qtl","r_90_qtl","r_95_qtl", "g_mean",
                                                "g_std","g_5_qtl","g_10_qtl","g_25_qtl","g_50_qtl",
                                                "g_75_qtl","g_90_qtl","g_95_qtl","b_mean","b_std",
                                                "b_5_qtl","b_10_qtl","b_25_qtl","b_50_qtl",	"b_75_qtl",
                                                "b_90_qtl","b_95_qtl","r_g_correl","g_b_correl","b_r_correl")
# Convert the date column to Date type
MLBS_under_UN_1000$date <- as.Date(MLBS_under_UN_1000$date, format="%Y-%m-%d")
# Filter the data from 2018 to 2022
filtered_under <- MLBS_under_UN_1000 %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31"))
View(filtered_under)

##Aggregate the data with the same dates into mean value
# Extract month and day from the date column
filtered_under <- filtered_under %>%
  mutate(month_day = format(date, "%m-%d"))
# Group by month and day and calculate the mean of the GCC column
aggregated_under <- filtered_under %>%
  group_by(month_day) %>%
  summarize(gcc = mean(gcc, na.rm = TRUE))
View(aggregated_under)

##obtain the trait of MLBS
MLBS_trait <- loadByProduct(dpID="DP1.10026.001", 
                            site= "MLBS",
                            startdate= NA,
                            enddate= NA)
View(MLBS_trait)

##obtain the ground phenology of MLBS
# download raw data (only 23 sites have data)
MLBS_Gphenology <- loadByProduct(dpID="DP1.10055.001", 
                           site="MLBS",
                           startdate= NA,
                           enddate= NA)
View(MLBS_Gphenology)
# extract phe_perindividual and join them as a table
MLBS_perindividual <- MLBS_Gphenology$phe_perindividual
# do same process for phe_statusintensity
MLBS_intensity <- MLBS_Gphenology$phe_statusintensity
# join the phe_perindividual and phe_statusintensity based on individual ID
MLBS_combined <- left_join(MLBS_perindividual, MLBS_intensity, by = "individualID")
View(MLBS_combined)



??get_pheno_ts
NEON.D07.MLBS.DP1.00042
NEON.D07.MLBS.DP1.00033
