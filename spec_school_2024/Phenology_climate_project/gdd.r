#Calculate gdd

suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
  }))

hpccPath <- '/Volumes/ersamlab/phenology/data'


bottom_temp <- read_csv(file.path(hpccPath,'bottom_irbio_temperature.csv'))
top_temp <- read_csv(file.path(hpccPath,'top_irbio_temperature.csv'))
  

