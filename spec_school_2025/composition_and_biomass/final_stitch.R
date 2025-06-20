# The final stitch
# This is a portion of the 'Composition and Biomass' project group
# Written by all the peeps
# Last updated 20250616

# Here, we're exploring to relationship of biomass to different variables
# Specifically, how do several different variables predict biomass?

# Set up the script by setting our working directory and loading in libraries
library(tidyverse)
library(readxl)

setwd("C:/Users/nblan/Desktop/Classes/SpecSchool/Repository/SPEC_School/spec_school_2025/composition_and_biomass")
wd <- getwd()

# Let's load in our different data sources

biomass <- read.csv(paste0(wd, "/biomass.csv")) %>%
  select(plot, biomass, sd, height, height_sd)

# We're going to also convert the plot names to match the biomass names
diversity <- read.csv(paste0(wd, "/taxonomic_diversity/MLBS_2023_div_metrics.csv")) %>%
  mutate(plotID = as.numeric(substr(plotID, nchar(plotID)-1, nchar(plotID)))) %>%
  rename(plot = plotID)

# read in the nitrogen data
nitrogen <- read_xlsx(paste0(wd, "/mlbs-nitrogen-predicted.xlsx")) %>%
  mutate(plotID = as.numeric(substr(plotID, nchar(plotID)-1, nchar(plotID)))) %>%
  rename(plot = plotID, 
         nitrogen = percentNitrogenPredicted)

# Create a full dataframe with all of the variables

columns <- c("plot", 
             "shannon_total", 
             "evenness_total", 
             "nspp_total", 
             "biomass",
             "sd",
             "nitrogen", 
             "height", 
             "height_sd")

data <- biomass %>%
  inner_join(diversity, by = "plot") %>%
  inner_join(nitrogen, by = "plot") %>%
  select(columns) %>%
  mutate(biomass = biomass/1000, 
         sd = sd/1000)

data %>%
  ggplot(aes(x = shannon_total, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = nspp_total, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = nitrogen, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = shannon_total, y = nitrogen)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = nspp_total, y = nitrogen)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = sd, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = sd, y = nitrogen)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = sd, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = height, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = height_sd, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = biomass, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Biomass and shannon index", 
    caption = "R^2 = **** off"
  )

data %>%
  ggplot(aes(x = nitrogen, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Nitrogen and shannon index", 
    caption = "R^2 = looking here is rude"
  )

data %>%
  ggplot(aes(x = height_sd, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Height SD and shannon index", 
    caption = "R^2 = ..l.."
  )

data %>%
  ggplot(aes(x = nspp_total, y = shannon_total)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Species richness and shannon index", 
    caption = "R^2 = ..l.."
  )



# Let's create some models

biomass_model_1 <- lm(biomass ~ nitrogen + shannon_total + nspp_total, data = data)
biomass_model_2 <- lm(biomass ~ nitrogen + shannon_total, data = data)
biomass_model_3 <- lm(biomass ~ nitrogen + nspp_total, data = data)
biomass_model_4 <- lm(biomass ~ shannon_total + nspp_total, data = data)
biomass_model_5 <- lm(biomass ~ nitrogen, data = data)
biomass_model_6 <- lm(biomass ~ nspp_total, data = data)
biomass_model_7 <- lm(biomass ~ shannon_total, data = data)
biomass_model_8 <- lm(biomass ~ shannon_total + sd, data = data)
biomass_model_9 <- lm(biomass ~ nitrogen + sd, data = data)
biomass_model_10 <- lm(biomass ~ sd, data = data)

summary(biomass_model_1)
summary(biomass_model_2)
summary(biomass_model_3)
summary(biomass_model_4)
summary(biomass_model_5)
summary(biomass_model_6)
summary(biomass_model_7)
summary(biomass_model_8)
summary(biomass_model_9)
summary(biomass_model_10)

# Now predicting nitrogen
nitrogen_model_1 <- lm(nitrogen ~ biomass + shannon_total + nspp_total, data = data)
nitrogen_model_2 <- lm(nitrogen ~ biomass + shannon_total, data = data)
nitrogen_model_3 <- lm(nitrogen ~ biomass + nspp_total, data = data)
nitrogen_model_4 <- lm(nitrogen ~ shannon_total + nspp_total, data = data)
nitrogen_model_5 <- lm(nitrogen ~ biomass, data = data)
nitrogen_model_6 <- lm(nitrogen ~ nspp_total, data = data)
nitrogen_model_7 <- lm(nitrogen ~ shannon_total, data = data)
nitrogen_model_8 <- lm(nitrogen ~ shannon_total + sd, data = data)
nitrogen_model_9 <- lm(nitrogen ~ biomass + sd, data = data)
nitrogen_model_10 <- lm(nitrogen ~ sd, data = data)


summary(nitrogen_model_1)
summary(nitrogen_model_2)
summary(nitrogen_model_3)
summary(nitrogen_model_4)
summary(nitrogen_model_5)
summary(nitrogen_model_6)
summary(nitrogen_model_7)
summary(nitrogen_model_8)
summary(nitrogen_model_9)
summary(nitrogen_model_10)

# Predicting diversity
shannon_model_1 <- lm(shannon_total ~ biomass + nitrogen + sd, data = data)
shannon_model_2 <- lm(shannon_total ~ biomass + nitrogen, data = data)
shannon_model_3 <- lm(shannon_total ~ biomass + sd, data = data)
shannon_model_4 <- lm(shannon_total ~ nitrogen + sd, data = data)
shannon_model_5 <- lm(shannon_total ~ biomass, data = data)
shannon_model_6 <- lm(shannon_total ~ nspp_total, data = data)
shannon_model_7 <- lm(shannon_total ~ sd, data = data)
shannon_model_8 <- lm(shannon_total ~ height_sd, data = data)
shannon_model_9 <- lm(shannon_total ~ biomass + height_sd, data = data)
shannon_model_10 <- lm(shannon_total ~ biomass + height_sd + nitrogen, data = data)
shannon_model_11 <- lm(shannon_total ~ height_sd + nitrogen, data = data)


summary(shannon_model_1)
summary(shannon_model_2) # this is the best one
summary(shannon_model_3)
summary(shannon_model_4)
summary(shannon_model_5)
summary(shannon_model_6)
summary(shannon_model_7)
summary(shannon_model_8)
summary(shannon_model_9)
summary(shannon_model_10)
summary(shannon_model_11)



