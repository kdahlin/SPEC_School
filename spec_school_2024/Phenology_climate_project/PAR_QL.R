suppressWarnings(
  suppressPackageStartupMessages({
    library(neonUtilities)
    library(tidyverse)
  }))

photoRad <- loadByProduct(dpID="DP1.00066.001", site="MLBS", 
                        startdate="2018-01", enddate="2022-12",
                        package="basic", timeIndex="30",
                        check.size = F)

View(photoRad)

list2env(photoRad, .GlobalEnv)

View(PARQL_30min)

PAR <- as_tibble(PARQL_30min)

unique(PAR$verticalPosition) #Check what vertical positions are available

PARAgg <- PAR %>%
  filter(verticalPosition %in% c('000') &
           finalQF==0 &
           !is.na(linePARMean)) %>%
  mutate(date=as.Date(startDateTime)) %>%
  group_by(verticalPosition,date) %>%
  summarize(
    PAR_mean=mean(linePARMean),
    PAR_min=min(linePARMean),
    PAR_max=max(linePARMean)
  ) %>%
  ungroup %>%
  arrange(verticalPosition,date)

hpccPath <- 'Y:/phenology/data'

#Save top-of-canopy temperatures
PARAgg %>%
  filter(verticalPosition=='000') %>%
  select(-verticalPosition) %>%
  write_csv(file.path(hpccPath,'PAR_ground.csv'))

