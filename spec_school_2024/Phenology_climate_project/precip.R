install.packages("neonUtilities")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("plotly")

# Load required libraries
library(neonUtilities)  # for accessing NEON data
library(ggplot2)  # for plotting
library(dplyr)  # for data munging
library(plotly)
library(lubridate)

wd <- '/Users/mkc2/Desktop/Spec_School/Precipitation'

setwd(wd)
# Define the product codes for secondary (DP1.00006.001) and throughfall (DP1.00007.001) precipitation
precip_code <- "DP1.00006.001"

# Download precipitation data
precip_data <- loadByProduct(
  dpID = precip_code,
  site = "MLBS",
  startdate = "2018-01",
  enddate = "2022-12",
  package="basic", timeIndex="30",
  check.size = FALSE
)
View(precip_data)
list2env(precip_data, .GlobalEnv)

# Load top-of canopy data 
# secondary represents top of canopy while throughfall represents understory
View(SECPRE_30min)
SECPRE <- as_tibble(SECPRE_30min)
unique(SECPRE$verticalPosition) #Check what vertical positions are available

SECPREAgg <- SECPRE %>%
  filter(verticalPosition %in% c('060') &
           secPrecipRangeQF==0 &
           !is.na(secPrecipBulk)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    SecPrecip_mean=mean(secPrecipBulk),
    SecPrecip_min=min(secPrecipBulk),
    SecPrecip_max=max(secPrecipBulk)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)
hpccPath <- 'Z:/phenology/data'

#Save top-of-canopy precipitation
SECPREAgg %>%
  filter(verticalPosition=='060') %>%
  select(-verticalPosition) %>%
  write.csv(file.path(hpccPath,'top_Precip.csv'))

#Load understory precipitation data
View(THRPRE_30min)
THRPRE <- as_tibble(THRPRE_30min)
unique(THRPRE$verticalPosition) #Check what vertical positions are available

THRPREAgg <- THRPRE %>%
  filter(verticalPosition %in% c('000') &
           TFPrecipRangeQF==0 &
           !is.na(TFPrecipBulk)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    TFPrecip_mean=mean(TFPrecipBulk),
    TFPrecip_min=min(TFPrecipBulk),
    TFPrecip_max=max(TFPrecipBulk)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)
hpccPath <- 'Z:/phenology/data'

#Save throughfall(Understory) precipitation
THRPREAgg %>%
  filter(verticalPosition=='000') %>%
  select(-verticalPosition) %>%
  write.csv(file.path(hpccPath,'under_Precip.csv'))

















