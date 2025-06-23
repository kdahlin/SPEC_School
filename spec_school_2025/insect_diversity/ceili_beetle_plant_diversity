library(lidR)
library(sf)
library(readr)
library(mapview)
library(neonUtilities)
library(dplyr)

setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_AOP_data/MLBS/2022x")

plots <- st_read("NEON_Beetle_Plots")

# Subset lidar to plot 
plot_lookup <- read_csv('mlbs_tile_lookup.csv') %>%
  mutate(tile = paste0(tile, 0)) %>%
  filter(plot_id %in% plots$plotID)

# All lidar files 
neon_lidar_files <- list.files('L1/DiscreteLidar/ClassifiedPointCloud/', full.names = T)
neon_lidar_files_downlaod2 <- list.files('plot_lidar/DP1.30003.001/neon-aop-products/2022/FullSite/D07/2022_MLBS_5/L1/DiscreteLidar/ClassifiedPointCloud/',
                                         full.names = TRUE)

all_lidar_files <- c(neon_lidar_files, neon_lidar_files_downlaod2)
all_lidar_files[grep(paste0(plot_lookup$tile, collapse = '|'), all_lidar_files)]

unique(plot_lookup$tile)

# These tiles have plots in them 
# 537000_4142000
# 538000_4142000
# 544000_4142000

unique(plot_lookup$tile)

# Read in data
this_tile <- readLAS(neon_lidar_files[grep('538000_4142000', neon_lidar_files)])

# Subset plots to plot in this tile 
this_plot <- plots %>%
  filter(plotID %in% c('MLBS_006')) %>%
  st_transform(., st_crs(this_tile))

# Clip lidar data to plot
clipped_lidar_data <- lidR::clip_roi(this_tile, this_plot)

#normalize point clouds
#DEM/DTM 
DTM <- grid_terrain(clipped_lidar_data, res = 10, algorithm = tin())
plot(DTM)

#normalize the heights
las.norm <- lidR::normalize_height(clipped_lidar_data, algorithm = tin(), na.rm = TRUE, method = "bilinear")
plot(las.norm@data$Z, main = "Normalized heights") 

#library(rgl)
options(rgl.useNULL = TRUE)
#x11()
plot(las.norm)

chm <- rasterize_canopy(las.norm, res = 1, algorithm = p2r())  # point-to-raster
plot(chm, main = "Canopy Height Model (Normalized)")


#get the basic metrics from the package
grid_metrics <- grid_metrics(las.norm, .stdmetrics, 30)

plot(grid_metrics$zmax)

mapview(grid_metrics$zentropy)

#foliar height diversity (percent lidar returns per 1 voxel)


###########################################
###########################################
#Beetle counts script
library(tidyr)
library(dplyr) #pipe pipe
library(mosaic)
library(tibble) #pivot widerrrr

setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_field_data/MLBS/2022/count_beetle")

# download beetle data
beetles <- loadByProduct(dpID = "DP1.10022.001",
                         site = c("MLBS","TREE","GRSM","ABBY","YELL", "TALL", "HARV", "BART", "ORNL"),
                         #year = 2022,
                         startdate = "2022-05",
                         enddate = "2022-08",
                         check.size = FALSE)

write.csv(beetles[["bet_expertTaxonomistIDProcessed"]], file = "allsite_count_beetles.csv")

#bring it on in! 
dat <- read.csv("allsite_count_beetles.csv")

#group them by plotID and scientific name 
counts_per_site <- 
  dat %>%
  group_by(plotID, scientificName) %>% #group by the plotID
  summarize(total.detections = n()) %>% #summarizing for number of detections 
  pivot_wider( 
   names_from = plotID,
   values_from = total.detections,
   values_fill = 0  #fill missing combinations with 0
  )

library(vegan) #need this one for the diversity() function
library(dplyr) 

community_matrix <- counts_per_site %>%
  column_to_rownames("scientificName") 
community_matrix_t <- t(community_matrix)


diversity_summary <- community_matrix_t %>%
  as.data.frame() %>%
  mutate(
    richness = specnumber(.),
    shannon = diversity(., index = "shannon"),
    simpson = diversity(., index = "simpson"),
    evenness = shannon / log(richness)
  )

diversity_summary <- diversity_summary %>%
  select(richness, shannon, simpson, evenness, everything())

write.csv(diversity_summary, "beetle_metrics_all.csv")

#alpha beta gamma
plot_site <- data.frame(
  plotID = rownames(community_matrix_t),
  siteID = gsub("_\\d+", "", rownames(community_matrix_t)) 
)

alpha_plot <- specnumber(community_matrix_t)

alpha_df <- data.frame(
  plotID = names(alpha_plot),
  richness = alpha_plot
) %>%
  left_join(plot_site, by = "plotID")

beetle_matrix_df <- as.data.frame(community_matrix_t)
beetle_matrix_df$plotID <- rownames(beetle_matrix_df)
beetle_long <- beetle_matrix_df %>%
  pivot_longer(-plotID, names_to = "species", values_to = "abund") %>%
  left_join(plot_site, by = "plotID")

beetle_long <- beetle_long %>%
  mutate(present = abund > 0)

gamma_by_site <- beetle_long %>%
  group_by(siteID, species) %>%
  summarise(present = any(present), .groups = "drop") %>%
  group_by(siteID) %>%
  summarise(gamma = sum(present))

alpha_site <- alpha_df %>%
  group_by(siteID) %>%
  summarise(mean_alpha = mean(richness))

site_diversity <- left_join(gamma_by_site, alpha_site, by = "siteID") %>%
  mutate(beta_whittaker = gamma / mean_alpha)


###############
#Plants########
#tbh would not trust this site to site

setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_field_data/MLBS/2022/plant_cover")

plants <- loadByProduct(dpID = "DP1.10058.001",
                         site = c("MLBS","TREE","GRSM","ABBY","YELL", "TALL", "HARV", "BART", "ORNL"),
                         #year = 2022,
                         startdate = "2022-06",
                         enddate = "2022-07",
                         check.size = FALSE)

write.csv(plants[["div_1m2Data"]], file = "allsite_plant_presence.csv")

#bring it on in! 
dat <- read.csv("allsite_plant_presence.csv")

plants <- dat %>%
  mutate(plotID = gsub("\\.basePlot\\.div", "", namedLocation))

setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_field_data/MLBS/2022")
beetles <- read.csv("count_beetle/beetle_metrics_all.csv")
colnames(beetles)[1] <- "plotID"
beetle_plotIDs <- unique(beetles$plotID)

plants_filtered <- plants %>%
  filter(plotID %in% beetle_plotIDs)

plant_div <- plants_filtered %>%
  select(plotID, subplotID, scientificName, percentCover) %>%
  filter(!is.na(scientificName), !is.na(percentCover))

species_matrix <- plant_div %>%
  group_by(plotID, scientificName) %>%
  summarise(totalCover = sum(percentCover), .groups = "drop") %>%
  pivot_wider(names_from = scientificName, values_from = totalCover, values_fill = 0) %>%
  column_to_rownames(var = "plotID")

library(vegan)
richness <- specnumber(species_matrix)
shannon <- diversity(species_matrix, index = "shannon")
evenness <- shannon / log(richness)

plant_metrics <- data.frame(
  plotID = rownames(species_matrix),
  richness = richness,
  shannon = shannon,
  evenness = evenness
)

cover_summary <- plant_div %>%
  group_by(plotID) %>%
  summarise(total_cover = sum(percentCover),
            mean_cover = mean(percentCover))

plant_summary <- plant_metrics %>%
  left_join(cover_summary, by = "plotID")

write.csv(plant_summary, "plant_cover/plant_summary_all.csv")

###alpha beta gamma shtuff 
plant_div <- dat %>%
  filter(!is.na(scientificName)) %>%
  mutate(subplot_uid = paste(siteID, plotID, subplotID, sep = "_"))

#species subplot
subplot_matrix <- plant_div %>%
  group_by(subplot_uid, scientificName) %>%
  summarise(present = 1, .groups = "drop") %>%
  pivot_wider(names_from = scientificName, values_from = present, values_fill = 0) %>%
  column_to_rownames("subplot_uid")

#get alpha
alpha_div <- specnumber(subplot_matrix)

subplot_meta <- dat %>%
  mutate(subplot_uid = paste(siteID, plotID, subplotID, sep = "_")) %>%
  select(subplot_uid, plotID, siteID) %>%
  distinct()

subplot_matrix_df <- as.data.frame(subplot_matrix)
subplot_matrix_df$subplot_uid <- rownames(subplot_matrix_df)

#plot info 
subplot_matrix_long <- left_join(subplot_matrix_df, subplot_meta, by = "subplot_uid")

#gamma div per plot
gamma_by_plot <- subplot_matrix_long %>%
  pivot_longer(cols = -c(subplot_uid, plotID, siteID), names_to = "species", values_to = "present") %>%
  group_by(plotID, species) %>%
  summarise(any_present = max(present), .groups = "drop") %>%
  group_by(plotID) %>%
  summarise(gamma_div = sum(any_present))

alpha_by_plot <- data.frame(
  subplot_uid = names(alpha_div),
  alpha_div = alpha_div
) %>%
  left_join(subplot_meta, by = "subplot_uid") %>%
  group_by(plotID) %>%
  summarise(mean_alpha = mean(alpha_div))

#combine to get beta
plot_diversity <- left_join(gamma_by_plot, alpha_by_plot, by = "plotID") %>%
  mutate(beta_div = gamma_div / mean_alpha)

write.csv(plot_diversity, "plant_cover/plant_ABG.csv")


##COMBINED
setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_AOP_data_MLBS_2022x")
structure_1 <- read.csv("lidar_metrics_2.csv")
structure_2 <- read.csv("lidar_metrics.csv")
structure <- rbind(structure_1, structure_2)

lidar_wide <- structure %>%
  rename(plotID = plot) %>%
  group_by(plotID, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = value)

setwd("/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_field_data/MLBS/2022")

beetles <- read.csv("count_beetle/beetle_metrics_all.csv")
colnames(beetles)[1] <- "plotID"
bugs <- beetles %>% 
  select(plotID, richness, shannon, evenness) %>% 
  rename("beetle_richness" = richness, "beetle_shannon" = shannon, "beetle_evenness" = evenness)

plants <- read.csv("plant_cover/plant_ABG.csv") %>% 
  select(plotID,gamma_div,mean_alpha,beta_div)


plants_insects <- bugs %>%
  left_join(plants, by = "plotID") %>% 
  mutate(site = sub("_.*", "", plotID))

proj_all <- plants_insects %>% 
  left_join(lidar_wide, by = "plotID") %>% 
  mutate(site = sub("_.*", "", plotID))
  

library(stringr)
df <- proj_all %>%
  mutate(site = str_extract(plotID, "^[^_]+")) %>%
  group_by(site) %>%
  summarise(
    beetle_richness = mean(beetle_richness, na.rm = TRUE),
    beetle_shannon = mean(beetle_shannon, na.rm = TRUE),
    gamma_div = mean(gamma_div, na.rm = TRUE),
    mean_alpha = mean(mean_alpha, na.rm = TRUE),
    beta_div = mean(beta_div, na.rm = TRUE),
    crr = mean(crr, na.rm = TRUE),
    fhd = mean(fhd, na.rm = TRUE),
    entropy = mean(entropy, na.rm = TRUE),
    top_rug = mean(top_rug, na.rm = TRUE)
  )

df_lidar <- lidar_wide %>%
  mutate(site = str_extract(plotID, "^[^_]+")) %>%
  group_by(site) %>%
  summarise(
    crr = mean(crr, na.rm = TRUE),
    fhd = mean(fhd, na.rm = TRUE),
    entropy = mean(entropy, na.rm = TRUE),
    top_rug = mean(top_rug, na.rm = TRUE)
  ) %>% 
  rename(siteID = site)

df_sites <- site_diversity %>% 
  left_join(df_lidar, by = "siteID") 
  

#idk let's look at relationships...
library(GGally)
GGally::ggpairs(proj_all %>% select(beetle_richness, beetle_shannon, gamma_div, mean_alpha, beta_div, fhd, entropy, top_rug, crr))

#might see more if we were to add sites and aggregate - would be quite nice to have more than 4 pts
GGally::ggpairs(df_sites %>% select(gamma, mean_alpha, beta_whittaker, fhd, entropy, top_rug, crr))

library(ggplot2)

proj_all %>%
  filter(is.na(beetle_richness) | is.na(crr))

ggplot(df_sites, aes(x = mean_alpha, y = crr)) +
  geom_point() + 
  geom_smooth(method = "lm")

mod <- lm(mean_alpha ~ top_rug, data = df_sites)
r2 <- summary(mod)$r.squared

ggplot(df_sites, aes(x = top_rug, y = mean_alpha)) +
  geom_point(size = 3) +
  geom_text(aes(label = siteID), vjust = -1.1, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 25,
          label = paste0("R² = ", round(r2, 3)),
          size = 4) +
  theme_minimal() +
  labs(
    title = "" ,
    x = "Average Plot Top Rugosity",
    y = "Average Plot Beetle Richness"
  )
