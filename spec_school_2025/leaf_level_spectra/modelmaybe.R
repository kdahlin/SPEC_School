getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
spectra<-read.csv("facet_spectra.csv")
getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/leaf_level_spectra")
weights<-read.csv("weight.only.csv")


averaged_spectra <- spectra %>%
  group_by(species, timestep, temp) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')

averaged_spectra<-averaged_spectra[-2,]
averaged_spectra<-averaged_spectra[-1,]

hist(averaged_spectra$avg_value)

library(MASS)
library(ggplot2)
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

ggplot(data=averaged_spectra, aes(x=avg_value, y=vec2))+geom_point()

rsq(averaged_spectra$avg_value, averaged_spectra$vec2)

