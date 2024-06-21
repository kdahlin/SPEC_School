#Calculate gdd

suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
  }))

hpccPath <- '/Volumes/ersamlab/phenology/data'

# These csvs were generated using IR_bio_temp.r 
bot_temp <- read_csv(file.path(hpccPath,'bottom_irbio_temperature.csv'))
top_temp <- read_csv(file.path(hpccPath,'top_irbio_temperature.csv'))

tdat <- bind_rows(bot_temp,top_temp)

# Calculate daily gdd
# Then accumulate from the beginning of the year

tbase <- 10

tdatGdd <- tdat %>%
  mutate(
    t_mean_gdd = (t_min + t_max)/2,
    gdd_daily = if_else(t_mean_gdd > tbase, t_mean_gdd - tbase, 0),
    year=year(date)
  ) %>%
  mutate(
    gdd_accum=cumsum(gdd_daily),
    .by=c(year,canopy_level)
  ) %>%
  select(canopy_level,verticalPosition,date,year,gdd_daily,gdd_accum)

# Save the dataset
tdatGdd %>%
  filter(canopy_level=='bottom_of_canopy') %>%
  select(-year) %>%
  write_csv(file.path(hpccPath,'bottom_gdd_accum.csv'))

tdatGdd %>%
  filter(canopy_level=='top_of_canopy') %>%
  select(-year) %>%
  write_csv(file.path(hpccPath,'top_gdd_accum.csv'))

