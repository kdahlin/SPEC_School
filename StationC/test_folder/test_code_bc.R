library(terra)
library(tidyverse)
library(ggplot2)

wd <- '~/Current Projects/SpecSchool/SPEC_School/StationC/'
setwd(wd)


plots <- vect('data/Kamoske_etal_2022_data/Kamoske_etal_2022_data/neon_plots_mlbs.shp')
agb <- rast('data/MLBS_agbEcoregion_20m.tif')
MLBS.data <- read.csv('data/Kamoske_etal_2022_data/Kamoske_etal_2022_data/all_metrics_20200803.csv') %>%
    filter(siteID == 'MLBS')


plot(agb)
plot(plots, add = T)
agb.points <- terra::extract(agb, plots, fun=mean, ID = F)
agb.points <- cbind.data.frame(plotID = plots$id, agb.points)
names(agb.points)[2] <- 'AGB'

MLBS.data <- merge(MLBS.data, agb.points, by = 'plotID')
summary(MLBS.data)

summary(lm(AGB ~ diversity_shannon, data = MLBS.data))
cor(MLBS.data$AGB, MLBS.data$diversity_shannon)

ggplot(MLBS.data) +
    geom_point(aes(x = diversity_shannon, y = AGB), size = 4, shape = 1, stroke = 1) +
    geom_abline(slope = 37.84, intercept = 125.09, linewidth = 1, color = 'red') +
    annotate('text', x = 1.7, y = 200, label = 'p = 0.132', size = 6, color = 'red') +
    theme_bw(base_size = 20) +
    labs(x = 'Shannons Diversity', y = 'AGB')



summary(lm(AGB ~ diversity_shannon, data = MLBS.data))
cor(MLBS.data$AGB, MLBS.data$diversity_shannon)

ggplot(MLBS.data) +
    geom_point(aes(x = diversity_shannon, y = AGB), size = 4, shape = 1, stroke = 1) +
    geom_abline(slope = 37.84, intercept = 125.09, linewidth = 1, color = 'red') +
    annotate('text', x = 1.7, y = 200, label = 'p = 0.132', size = 6, color = 'red') +
    theme_bw(base_size = 20) +
    labs(x = 'Shannons Diversity', y = 'AGB')

