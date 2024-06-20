suppressWarnings(
  suppressPackageStartupMessages({
    library(neonUtilities)
    library(tidyverse)
  }))

photoRad <- loadByProduct(dpID="DP1.00024.001", site="MLBS", 
                        startdate="2018-01", enddate="2022-12",
                        package="basic", timeIndex="30",
                        check.size = F)

View(photoRad)

list2env(photoRad, .GlobalEnv)

View(PARPAR_30min)

PAR <- as_tibble(PARPAR_30min)

unique(PAR$verticalPosition) #Check what vertical positions are available

PARAgg <- PAR %>%
  filter(verticalPosition %in% c('060') &
           PARFinalQF==0 &
           !is.na(PARMean)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    PAR_mean=mean(PARMean),
    PAR_min=min(PARMean),
    PAR_max=max(PARMean)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)

hpccPath <- 'Y:/phenology/data'

#Save top-of-canopy temperatures
PARAgg %>%
  filter(verticalPosition=='060') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'PAR_top.csv'))

