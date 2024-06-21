
hpccPath <- '/Volumes/ersamlab/phenology/data'
hpccPlots <- '/Volumes/ersamlab/phenology/figs'

# Growing degree days
.botF <- 'bottom_gdd_accum.csv'
.topF <- 'top_gdd_accum.csv'
.ylab <- 'Accumulated growing degree days'
.outF <- 'gdd_accum.png'

# Mean Relative humidity
.botF <- 'bottom_RH.csv'
.topF <- 'top_RH.csv'
.ylab <- 'Mean relative humidity (%)'
.outF <- 'mean_rh.png'
.yvar <- 'RH_mean'

# Mean PAR
.botF <- 'PAR_ground.csv'
.topF <- 'PAR_top.csv'
.ylab <- 'Mean Photo. Active Rad. (μM/m^2/s)'
.outF <- 'par_mean.png'
.yvar <- 'PAR_mean'

# Mean Temperature
.botF <- 'bottom_irbio_temperature.csv'
.topF <- 'top_irbio_temperature.csv'
.ylab <- 'Mean Temperature (°C)'
.outF <- 'temp_mean.png'
.yvar <- 't_mean'

# Mean Precip
.botF <- 'under_Precip.csv'
.topF <- 'top_Precip.csv'
.ylab <- 'Mean Precipitation (mm)'
.outF <- 'precip_mean.png'
.yvar <- 'precip_mean'

#Load the datasets. Add canopy level if it is missing.

#---- Understory
bot_dat <- read_csv(file.path(hpccPath,.botF))

if(!'canopy_level' %in% names(bot_dat)) {
  bot_dat$canopy_level <- 'bottom_of_canopy'
}

if('TFPrecip_mean' %in% names(bot_dat)) {
  bot_dat <- bot_dat %>% rename(precip_mean=TFPrecip_mean)
}

#---- Overstory
top_dat <- read_csv(file.path(hpccPath,.topF))

if(!'canopy_level' %in% names(top_dat)) {
  top_dat$canopy_level <- 'top_of_canopy'
}

if('SecPrecip_mean' %in% names(top_dat)) {
  top_dat <- top_dat %>% rename(precip_mean=SecPrecip_mean)
}

dat <- bind_rows(bot_dat,top_dat) %>%
  mutate(year=year(date))

levels=c("bottom_of_canopy","top_of_canopy")
labels=c('Understory','Overstory')

p <- dat %>%
  mutate(canopy_level=factor(canopy_level,levels=levels,labels=labels)) %>%
ggplot(aes(x=date,y=!!sym(.yvar),color=canopy_level,group=canopy_level)) +
geom_line(linewidth=1.5) +
scale_x_date(date_labels='%b') +
facet_wrap(vars(year),scales='free_x') +
theme_minimal() +
labs(y=.ylab,color=NULL,x=NULL) +
theme(plot.background = element_rect('white')) +
theme(
  plot.title = element_text(size=20),
  plot.tag = element_text(size=18),
  axis.text=element_text(size=14),
  axis.title=element_text(size=18),
  legend.text = element_text(size=18),
  legend.title = element_text(size=18),
  strip.text=element_text(size=18),
  legend.position = 'bottom'
); p

ggsave(file.path(hpccPlots,.outF),plot=p)
