install.packages("neonUtilities")
library(neonUtilities)
library(dplyr)
library(data.table)
library(phenocamapi)
library(lubridate)
library(jpeg)

setwd("E:\\Prof. Qiu Lab\\2024SPECschool\\SPEC_School\\spec_school_2024\\Phenology_climate_project")

##########################################################################################
####Hanshi will process the PhenoCam data (time series: 1 day)############################
##########################################################################################

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
MLBS_over_DB_2000 <- MLBS_over_DB_2000 %>%
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
colnames(MLBS_over_DB_2000) <-   c("date", "local_std_time", "doy", "filename",
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
MLBS_over_DB_2000$date <- as.Date(MLBS_over_DB_2000$date, format="%Y-%m-%d")

# Filter the data from 2018 to 2022
filtered_under <- MLBS_under_UN_1000 %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31"))
filtered_over <- MLBS_over_DB_2000 %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2022-12-31"))

##Aggregate the data with the same dates into mean value
# Extract month and day from the date column
filtered_under <- filtered_under %>%
  mutate(month_day_under = format(date, "%Y-%m-%d"))
# Group by month and day and calculate the mean of the GCC column
aggregated_under <- filtered_under %>%
  group_by(month_day_under) %>%
  summarize(gcc = mean(gcc, na.rm = TRUE))
View(aggregated_under)

# Extract month and day from the date column
filtered_over <- filtered_over %>%
  mutate(month_day_over = format(date, "%Y-%m-%d"))
# Group by month and day and calculate the mean of the GCC column
aggregated_over <- filtered_over %>%
  group_by(month_day_over) %>%
  summarize(gcc = mean(gcc, na.rm = TRUE))
View(aggregated_over)

###read Ben's temperature data
Temp_over <- read.csv("Y:\\phenology\\data\\top_irbio_temperature.csv")
Temp_under <- read.csv("Y:\\phenology\\data\\bottom_irbio_temperature.csv")
View(Temp_over)
View(Temp_under)

##before join, changing the common columes of two to be joined table
# Rename the column 'xx' to 'date'
aggregated_over <- aggregated_over %>%
  rename(date = month_day_over)
aggregated_under <- aggregated_under %>%
  rename(date = month_day_under)
# join the table
aggregated_over_temp <- left_join(aggregated_over, Temp_over, by = "date")
View(aggregated_over_temp)
aggregated_over_temp <- aggregated_over_temp %>%
  select(-canopy_level, -verticalPosition)
View(aggregated_over_temp)

aggregated_under_temp <- left_join(aggregated_under, Temp_under, by = "date")
View(aggregated_under_temp)
aggregated_under_temp <- aggregated_under_temp %>%
  select(-canopy_level, -verticalPosition)
View(aggregated_under_temp)

###read Basant's humidity (%) data
Humidity_over <- read.csv("Y:\\phenology\\data\\top_RH.csv")
Humidity_under <- read.csv("Y:\\phenology\\data\\bottom_RH.csv")
View(Humidity_over)
View(Humidity_under)

# join the table
aggregated_over_temp_humid <- left_join(aggregated_over_temp, Humidity_over, by = "date")
View(aggregated_over_temp_humid)

aggregated_under_temp_humid <- left_join(aggregated_under_temp, Humidity_under, by = "date")
View(aggregated_under_temp_humid)

###read Manisha's prcp (mm) data
Prcp_over <- read.csv("Y:\\phenology\\data\\top_precip.csv")
Prcp_under <- read.csv("Y:\\phenology\\data\\under_precip.csv")
View(Prcp_over)
View(Prcp_under)

# join the table
aggregated_over_temp_humid_prcp <- left_join(aggregated_over_temp_humid, Prcp_over, by = "date")
View(aggregated_over_temp_humid_prcp)
aggregated_under_temp_humid_prcp <- left_join(aggregated_under_temp_humid, Prcp_under, by = "date")
View(aggregated_under_temp_humid_prcp)

##delete the unnecessary columes
df_over_THP <- aggregated_over_temp_humid_prcp %>%
  select(-X)
View(df_over_THP)
df_under_THP <- aggregated_under_temp_humid_prcp %>%
  select(-X)
View(df_over_THP)
View(df_under_THP)

###read Ben's GDD data
GDD_over <- read.csv("Y:\\phenology\\data\\top_gdd_accum.csv")
GDD_under <- read.csv("Y:\\phenology\\data\\bottom_gdd_accum.csv")
View(GDD_over)
View(GDD_under)

# join the table
df_over_THP_gdd <- left_join(df_over_THP, GDD_over, by = "date")
View(df_over_THP_gdd)
df_over_THP_gdd <- df_over_THP_gdd%>%
  select(-canopy_level, -verticalPosition)
View(df_over_THP_gdd)

df_under_THP_gdd <- left_join(df_under_THP, GDD_under, by = "date")
View(df_under_THP_gdd)
df_under_THP_gdd <- df_under_THP_gdd%>%
  select(-canopy_level, -verticalPosition)
View(df_under_THP_gdd)

##########################################################################################
####Hanshi will process the table ground trait data ############################
##########################################################################################

##obtain the trait of MLBS
MLBS_trait <- loadByProduct(dpID="DP1.10026.001", 
                            site= "MLBS",
                            startdate= "2018-01-01",
                            enddate= "2022-12-31")
Trait_lists <- list(MLBS_trait$cfc_fieldData,
                    MLBS_trait$cfc_carbonNitrogen, 
                    MLBS_trait$cfc_chlorophyll,
                    MLBS_trait$cfc_elements,
                    MLBS_trait$cfc_lignin,
                    MLBS_trait$cfc_LMA)
MLBS_trait_1 <- Reduce(function(x, y) left_join(x, y, by = "sampleID"), Trait_lists)
View(MLBS_trait_1)

MLBS_trait_summary <- MLBS_trait_1 %>%
  group_by(scientificName) %>%
  summarize(
    C_nitrogenPercent = mean(nitrogenPercent, na.rm = TRUE),
    C_carbonPercent = mean(carbonPercent, na.rm = TRUE),
    C_CNratio = mean(CNratio, na.rm = TRUE),
    CHL_extractChlAConc = mean(extractChlAConc, na.rm = TRUE),
    CHL_extractChlBConc = mean(extractChlBConc, na.rm = TRUE),
    CHL_extractCarotConc = mean(extractCarotConc, na.rm = TRUE),
    E_foliarPhosphorusConc = mean(foliarPhosphorusConc, na.rm = TRUE),
    E_foliarPotassiumConc = mean(foliarPotassiumConc, na.rm = TRUE),
    L_dryMass = mean(dryMass, na.rm = TRUE),
    L_ligninPercent = mean(ligninPercent, na.rm = TRUE),
    L_cellulosePercent = mean(cellulosePercent, na.rm = TRUE),
    LMA_leafMassPerArea = mean(leafMassPerArea, na.rm = TRUE),
    collectionDate = first(collectDate.x)  # Assuming you want the first collection date
  ) %>%
  select(scientificName, 
         C_nitrogenPercent, C_carbonPercent, C_CNratio, 
         CHL_extractChlAConc, CHL_extractChlBConc, CHL_extractCarotConc,
         E_foliarPhosphorusConc, E_foliarPotassiumConc,
         L_dryMass, L_ligninPercent, L_cellulosePercent,
         LMA_leafMassPerArea, 
         collectionDate)
View(MLBS_trait_summary)

##obtain the ground phenology of MLBS
# download raw data of MLBS
MLBS_Gphenology <- loadByProduct(dpID="DP1.10055.001", 
                           site="MLBS",
                           startdate= "2018-01-01",
                           enddate= "2018-12-31")
View(MLBS_Gphenology)
# extract phe_perindividual and join them as a table
MLBS_perindividual <- MLBS_Gphenology$phe_perindividual
# do same process for phe_statusintensity
MLBS_intensity <- MLBS_Gphenology$phe_statusintensity
# join the phe_perindividual and phe_statusintensity based on individual ID
MLBS_combined <- left_join(MLBS_perindividual, MLBS_intensity, by = "individualID")
View(MLBS_combined)

