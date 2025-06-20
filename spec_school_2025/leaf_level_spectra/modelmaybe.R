getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
spectra<-read.csv("facet_spectra.csv")
getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/leaf_level_spectra")
weights<-read.csv("weight.only.csv")

library(MASS)
library(ggplot2)
library(ggpmisc)
library(tidyverse)


averaged_spectra <- spectra %>%
  group_by(species, timestep, temp) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')

averaged_spectra<-averaged_spectra[-2,]
averaged_spectra<-averaged_spectra[-1,]

write.csv(averaged_spectra, "averaged_spectra.csv")

averaged_spectra<-read.csv("averaged_spectra.csv")

hist(averaged_spectra$avg_value)

rsq <- function (x, y) cor(x, y) ^ 2

#first model
one<-glm.nb(avg_value~species+timestep+temp, data=averaged_spectra)

warnings(one)
plot(one$residuals)
summary(one)

vec<-one$fitted.values

averaged_spectra$vec<-vec

ggplot(data=averaged_spectra, aes(x=avg_value, y=vec))+geom_point()

rsq(averaged_spectra$avg_value, averaged_spectra$vec)

#second model
two<-glm(avg_value~species+timestep+temp, data=averaged_spectra)

plot(two$residuals)
summary(two)

vec2<-two$fitted.values

averaged_spectra$vec2<-vec2

rsq(averaged_spectra$avg_value, averaged_spectra$vec2)

ggplot(data = averaged_spectra, aes(x = avg_value, y = vec2)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, color = "orange") +
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(x = "Measured", y = "Predicted", title = "Reflectance Model") +
  theme_bw()

#third model
hist(averaged_spectra$LWC)
three<-glm(LWC~avg_value+species+timestep+temp, data=averaged_spectra)

plot(three$residuals)
summary(three)

vec3<-three$fitted.values

averaged_spectra$vec3<-vec3

rsq(averaged_spectra$avg_value, averaged_spectra$vec3)

ggplot(data = averaged_spectra, aes(x = LWC, y = vec3)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, color = "green") +
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(x = "Measured", y = "Predicted", title = "LWC Model") +
  theme_bw()


#fourth model
hist(averaged_spectra$RWC)
four<-glm(RWC~avg_value+species+timestep+temp, data=averaged_spectra)

plot(four$residuals)
summary(four)

vec3<-three$fitted.values

averaged_spectra$vec3<-vec3

rsq(averaged_spectra$avg_value, averaged_spectra$vec3)

ggplot(data = averaged_spectra, aes(x = LWC, y = vec3)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, color = "green") +
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(x = "Measured", y = "Predicted", title = "LWC Model") +
  theme_bw()


