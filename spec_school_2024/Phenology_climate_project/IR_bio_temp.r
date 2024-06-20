
#Downloads IR_bio_temp product and aggregates to daily min, mean, max
# Follows this tutorial: https://www.neonscience.org/resources/learning-hub/tutorials/neon-saat-temp-r

suppressWarnings(
  suppressPackageStartupMessages({
    library(neonUtilities)
    library(tidyverse)
  }))

irbioPkg <- loadByProduct(dpID="DP1.00005.001", site="MLBS", 
                      startdate="2018-01", enddate="2022-12",
                      package="basic", timeIndex="30",
                      check.size = F)

saveRDS(irbioPkg,'/Users/benc/projects/spec/data/ir_bio_temp.rds')

View(irbioPkg)

list2env(irbioPkg, .GlobalEnv)

View(IRBT_30_minute)

irbio <- as_tibble(IRBT_30_minute)

unique(irbio$verticalPosition) #Check what vertical positions are available

irbioAgg <- irbio %>%
  filter(verticalPosition %in% c('000','040') &
        finalQF==0 &
        !is.na(bioTempMean)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    t_mean=mean(bioTempMean),
    t_min=min(bioTempMean),
    t_max=max(bioTempMean)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)

hpccPath <- '/Volumes/ersamlab/phenology/data'

#Save top-of-canopy temperatures
irbioAgg %>%
  filter(verticalPosition=='000') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'top_irbio_temperature.csv'))

#Save top-of-canopy temperatures
irbioAgg %>%
  filter(verticalPosition=='040') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'bottom_irbio_temperature.csv'))
  
