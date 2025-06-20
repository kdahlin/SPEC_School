# join unique per plot
fc_joined <- fc_filtered_unique %>%
  left_join(plots_unique, by = "plotID") %>%
  st_as_sf() %>%
  st_transform(crs = h5CRS)

fc_vect <- vect(fc_joined)

plot_spectra <- terra::extract(hsStack, fc_vect, fun = mean, na.rm = TRUE)

spectra_with_traits <- cbind(fc_filtered_unique, plot_spectra[,-1])

spectra_with_traits <- spectra_with_traits %>%
  select(where(~ !all(is.na(.))))

glimpse(spectra_with_traits)

# check plots w/ nitrogen data over hs tile
plot(hsStack[[1]], main = "Plots in tile (Band 1)")
plot(fc_vect, add = TRUE, col = "red", pch = 16)
text(fc_vect, labels = fc_vect$plotID, pos = 3, cex = 0.8, col = "white")

as.data.frame(geom(fc_vect))
