library(terra)
library(tidyverse)
library(ggplot2)
library(tidyterra)

# file paths
proj.path <- file.path('~/Current Projects/SpecSchool/SPEC_School/N_fixer_project/')
data.path <- file.path(proj.path, 'data')
# data paths
N.map.path <- file.path('~/Current Projects/SpecSchool/MLBS_nitrogen_mean_sd_cv_2017.tif')
AGB.path <- file.path(data.path, 'MLBS_agbEcoregion_20m.tif')
Nfix.points.path <- file.path(data.path, 'Point_ge.shp')
plot.path <- file.path(data.path, 'neon_plots_mlbs.shp')


# load spatial data -------------------------------------------------------

# Raster data
MLBS.N <- rast(N.map.path)
MLBS.N2 <- terra::focal(
    x = MLBS.N,
    w = 15, # sets a 5 cell window around NA pixel
    fun = "mean",
    na.policy = "only", # only interpolate NA values
    na.rm = T,
    overwrite = TRUE
)
MLBS.AGB <- rast(AGB.path)

# Vector data
nfixers <- vect(Nfix.points.path)
plots <- vect(plot.path)

plot(MLBS.N$nitrogen_mean)
plot(nfixers, add = T)
plot(plots, add = T)
plots$id <- as.numeric(sapply(str_split(plots$id, '_'), "[[", 2))

ggplot() +
    geom_spatraster(data = MLBS.N2, aes(fill=nitrogen_mean)) +
    scale_fill_viridis_c(na.value = 'transparent', option = 'A') +
    geom_spatvector(data = plots, fill = 'red', color = 'red', size = 15) +
    geom_spatvector_text(data = plots,
                         aes(label = id),
                         color = "black",
                         check_overlap = F) +
    theme_bw(base_size = 10)



# extract N data ----------------------------------------------------------
# From the plots
plot.N <- extract(MLBS.N, plots, fun=mean, na.rm = T)




# From random points
# Nfix.N <- extract(MLBS.N2, nfixers, fun=mean, na.rm = T)
nrun <- 1000
Nfix.N <- cbind.data.frame(Nfix = 'Nfix', extract(MLBS.N2, buffer(nfixers, 5), fun=mean, na.rm=T))
for( i in seq(nrun)) {
    Rand.N <- cbind.data.frame(Nfix = 'Rand', ID = i, spatSample(MLBS.N2, 15, na.rm = F))
    Nfix.N <- rbind.data.frame(Nfix.N, Rand.N)
}


# plotting ----------------------------------------------------------------

Nfix.N.plot <- Nfix.N %>%
    group_by(Nfix, ID) %>%
    summarise(Mean = mean(nitrogen_mean, na.rm = T),
              SD = mean(nitrogen_sd, na.rm = T),
              CV = mean(nitrogen_cv, na.rm = T)) %>%
    pivot_longer(-c(Nfix,ID), names_to = 'Stat') %>%
    mutate(Stat = factor(Stat, levels = c('Mean', 'SD', 'CV')))

ggplot(Nfix.N.plot) +
    geom_boxplot(aes(x = Nfix, y = value, fill = Nfix), color = 'black') +
    scale_fill_manual(values = c('forestgreen', 'grey80')) +
    facet_wrap(.~Stat, scales = "free_y") +
    theme_bw(base_size = 20) +
    labs(x = '', y = 'Value')


t.test(Nfix.N$nitrogen_mean ~ Nfix.N$Nfix)
t.test(Nfix.N$nitrogen_sd ~ Nfix.N$Nfix)
t.test(Nfix.N$nitrogen_cv ~ Nfix.N$Nfix)
