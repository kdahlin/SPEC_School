

library(sf)
library(dplyr)

# 'All_NEON_TOS_Plot_Points_V11.shp'
plot_sf <- st_read('X:/shared_data/NEON_field_data/MLBS/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Points_V11.shp')
field_df <- read.csv("X:/shared_data/NEON_field_data/MLBS/NEONForestAGBv2_MLBS.csv")
length(unique(field_df$plotID)) #36

dim(field_df)
head(field_df)
names(plot_sf)
names(field_df)
as.factor(plot_sf$subtype)

plot_list <- field_df %>%
  distinct(plotID) %>%
  pull(plotID)

plot_sf_sub <- plot_sf %>%
  filter(plotID %in% plot_list)

length(unique(plot_sf_sub$plotID))
dim(plot_sf_sub)
names(plot_sf_sub)

write.csv(plot_sf_sub,"X:/shared_data/NEON_field_data/MLBS/NEON_TOP_Plot_Points_MLBS.csv")

# 1. Compute plot center
plot_sf_sub <- read.csv("X:/shared_data/NEON_field_data/MLBS/NEON_TOP_Plot_Points_MLBS.csv")
names(plot_sf_sub)
unique(plot_sf_sub$X) #36
dim(plot_sf_sub)
head(plot_sf_sub)
# 
# plot_center <- plot_sf_sub %>%
#   group_by(plotID) %>%
#   summarise(
#     geometry = st_centroid(st_union(geometry)),
#     easting = first(easting),
#     northing = first(northing),
#     .groups = "drop"
#   )


plot_center <- plot_sf_sub %>%
  group_by(plotID) %>%
  summarise(
    center_easting = mean(easting, na.rm = TRUE),
    center_northing = mean(northing, na.rm = TRUE),
    # center_lat = mean(latitude, na.rm = TRUE),
    # center_lon = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

dim(plot_center)
head(plot_center)
unique(plot_center$X)

# 2. Convert to sf
plot_center_sf <- st_as_sf(
  plot_center,
  coords = c("easting", "northing"),
  crs = 32617
)

# 3. Create plot area:buffer circle
plot_buffer <- st_buffer(plot_center_sf, dist = 20)
# st_buffer(plot_center_sf, dist = 10) |> st_bbox() |> st_as_sfc()

# # convex hull from the 4 points
# plot_polygon <- plot_sf %>%
#   group_by(plotID) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_convex_hull()
str(plot_buffer)
plot(plot_buffer["plotID"])

# filter plot_buffer to AOI by y < 413800
# filter tiles X:[541000:543000];Y:[413500:413800] 2*4=8 tiles
plot(plot_buffer["plotID"])

plot(st_geometry(plot_buffer),
     col = NA,
     border = "red",
     lwd = 2,
     axes = TRUE,
     graticule = TRUE)

# 4. Plot
crs(mosaic)
st_crs(plot_buffer)
plot_buffer <- st_transform(plot_buffer, crs(mosaic))
plot_vect <- vect(plot_buffer)

# RGB image
plotRGB(
  mosaic,
  r = 3, g = 2, b = 1,
  stretch = "lin"
)

# overlay plots
plot(
  plot_vect,
  add = TRUE,
  border = "red",
  lwd = 2
)

# 5. Save
st_write(plot_buffer,
         # "C:/YUWEN PANG/Work/SpecSchool/NEON_TOP_Plot_20mbuffer_MLBS.shp",
         'X:/shared_data/NEON_field_data/MLBS/NEON_TOP_Plot_20mbuffer_MLBS.shp',
         delete_dsn = TRUE)

shp <- st_read('X:/shared_data/NEON_field_data/MLBS/NEON_TOP_Plot_20mbuffer_MLBS.shp')
print(shp)
head(shp)
st_geometry(shp)
dim(shp) #36*4

plot(
  shp,
  add = TRUE,
  border = "red",
  lwd = 2
)