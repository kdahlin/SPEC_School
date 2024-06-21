
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



# Plotting function
plot_RH <- function(data, title) {
  ggplot(data, aes(x = date)) +
    geom_line(aes(y = RH_mean, color = 'Mean')) +
    geom_line(aes(y = RH_min, color = 'Min')) +
    geom_line(aes(y = RH_max, color = 'Max')) +
    labs(
      title = title,
      x = 'Date',
      y = 'Relative Humidity (%)',
      color = 'Legend'
    ) +
    theme_minimal()
}

# Filter data for plotting
top_RH <- RHAgg %>% filter(verticalPosition == '060')
bottom_RH <- RHAgg %>% filter(verticalPosition == '000')

# Create plots
plot_top_RH <- plot_RH(top_RH, 'Top-of-Canopy Relative Humidity')
plot_bottom_RH <- plot_RH(bottom_RH, 'Bottom-of-Canopy Relative Humidity')

# Display plots separately
plot_top_RH
plot_bottom_RH

install.packages("cowplot")

# Combine the plots for comparison
combined_plot <- cowplot::plot_grid(plot_top_RH, plot_bottom_RH, ncol = 1)

# Save the combined plot as a high-quality PNG
output_path <- file.path(hpccPath, 'combined_RH_plot.jpeg')
ggsave(output_path, plot = combined_plot, width = 10, height = 8, dpi = 300)

# Print the combined plot
print(combined_plot)
