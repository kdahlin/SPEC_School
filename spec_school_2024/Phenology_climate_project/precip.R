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
View(SECPRE_30min)
SECPRE <- as_tibble(SECPRE_30min)
unique(RH$verticalPosition) #Check what vertical positions are available

RHAgg <- RH %>%
  filter(verticalPosition %in% c('000','060') &
           RHFinalQF==0 &
           !is.na(RHMean)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    RH_mean=mean(RHMean),
    RH_min=min(RHMean),
    RH_max=max(RHMean)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)






# Extract relevant data from the list
precip_data_extracted <- precip_data$SECPRE_30min


list2env()
# Check the structure of the extracted data
str(precip_data_extracted)

# Ensure the data is not NULL
if (is.null(precip_data_extracted)) stop("Precipitation data not found")

# Assuming the relevant columns are 'startDateTime' and 'priPrecipBulk'
daily_precip <- precip_data_extracted %>%
  mutate(date = as_date(startDateTime)) %>%
  group_by(date) %>%
  summarize(daily_precip = sum(priPrecipBulk, na.rm = TRUE)) %>%
  ungroup()

write.csv(daily_precip, "daily_precip_2018_2022.csv", row.names = FALSE)





