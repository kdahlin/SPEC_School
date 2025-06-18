setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
specs<-read.csv("MLBS2025_SPEC_School_spectra_final.csv")

#calculate indices
#split into groups
library(ggplot2)
library(tidyverse)

specs2<-pivot_longer(specs, cols=3:97 ,names_to="trt", values_to="value")

ggplot(data=specs2, aes(x=wavelength, y=value, color=trt))+geom_line()

timestep<-substr(specs2$trt, start=1, stop=1)
species<-substr(specs2$trt, start=2, stop=3)
individual<-substr(specs2$trt, start=4, stop=4)
temp<-substr(specs2$trt, start=6, stop=6)
specs2<-mutate(specs2, timestep=timestep, species=species, individual=individual, temp=temp)

?substr
?mutate

getwd()
write.csv(specs2, "facet_spectra.csv")

