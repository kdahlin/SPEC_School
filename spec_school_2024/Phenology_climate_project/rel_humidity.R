
#Downloads IR_bio_temp product and aggregates to daily min, mean, max
# Follows this tutorial: https://www.neonscience.org/resources/learning-hub/tutorials/neon-saat-temp-r

suppressWarnings(
  suppressPackageStartupMessages({
    library(neonUtilities)
    library(tidyverse)
  }))

relhum <- loadByProduct(dpID="DP1.00098.001", site="MLBS", 
                          startdate="2018-01", enddate="2022-12",
                          package="basic", timeIndex="30",
                          check.size = F)

View(relhum)

list2env(relhum, .GlobalEnv)

View(RH_30min)

RH <- as_tibble(RH_30min)

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

hpccPath <- 'Y:/phenology/data'

#Save top-of-canopy temperatures
RHAgg %>%
  filter(verticalPosition=='060') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'top_RH.csv'))

#Save top-of-canopy temperatures
RHAgg %>%
  filter(verticalPosition=='000') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'bottom_RH.csv'))

