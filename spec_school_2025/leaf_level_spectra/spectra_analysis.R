setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
specs<-read.csv("MLBS2025_SPEC_School_spectra_final.csv")

#calculate indices
#split into groups
library(ggplot2)
library(tidyverse)

specs2 <- specs %>%
  pivot_longer(
    cols = -wavelength,
    names_to = "label",
    values_to = "value"
  )
head(specs2)


specs2 <- specs2 %>% 
  mutate(timestep = substr(label,1,1),
         species = substr(label,2,3),
         individual = substr(label,4,4),
         temp = substr(label,6,6))

specs2<-specs2[-1,]


getwd()
write.csv(specs2, "facet_spectra.csv")

tcolor<-c("#154360", "#FF5733", "#FFC300", "#1ABC9C")
trtcolor<-c("red","blue")

#by species and timestep
BOs <-subset(specs2, species == "BO")
ggplot(data=BOs, aes(x=wavelength, y=value, color=timestep))+geom_line(linewidth=0.5)+
  scale_color_manual(values = tcolor)+
  facet_wrap(~ timestep, scales = "free_y")

WOs<-subset(specs2, species == "WO")
ggplot(data=WOs, aes(x=wavelength, y=value, color=timestep))+geom_line(linewidth=0.5)+
  scale_color_manual(values = tcolor)+
  facet_wrap(~ timestep, scales = "free_y")

RMs<-subset(specs2, species == "RM")
ggplot(data=RMs, aes(x=wavelength, y=value, color=timestep))+geom_line(linewidth=0.5)+
  scale_color_manual(values = tcolor)+
  facet_wrap(~ timestep, scales = "free_y")

SMs<-subset(specs2, species == "SM")
ggplot(data=SMs,aes(x=wavelength, y=value, color=timestep))+geom_line(linewidth=0.5)+
  scale_color_manual(values = tcolor)+
  facet_wrap(~ timestep, scales = "free_y")


trtcolor<-c("blue", "red")
#by species and treatment
BOs <-subset(specs2, species == "BO")
ggplot(data=BOs, aes(x=wavelength, y=value, color=temp))+geom_line(linewidth=0.5)+
  scale_color_manual(values = trtcolor)+facet_wrap(~ temp, scales = "free_y")

WOs<-subset(specs2, species == "WO")
ggplot(data=WOs, aes(x=wavelength, y=value, color=temp))+geom_line(linewidth=0.5)+
  scale_color_manual(values = trtcolor)+facet_wrap(~ temp, scales = "free_y")

RMs<-subset(specs2, species == "RM")
ggplot(data=RMs, aes(x=wavelength, y=value, color=temp))+geom_line(linewidth=0.5)+
  scale_color_manual(values = trtcolor)+facet_wrap(~ temp, scales = "free_y")

SMs<-subset(specs2, species == "SM")
ggplot(data=SMs,aes(x=wavelength, y=value, color=temp))+geom_line(linewidth=0.5)+
  scale_color_manual(values = trtcolor)+facet_wrap(~ temp, scales = "free_y")       


indcolor<-c("green", "yellow", "orange")
BOs <-subset(specs2, species == "BO")
ggplot(data=BOs, aes(x=wavelength, y=value, color=individual))+geom_line(linewidth=0.5)+
  scale_color_manual(values = indcolor)+facet_wrap(~ individual, scales = "free_y")

WOs<-subset(specs2, species == "WO")
ggplot(data=WOs,  aes(x=wavelength, y=value, color=individual))+geom_line(linewidth=0.5)+
  scale_color_manual(values = indcolor)+facet_wrap(~ individual, scales = "free_y")

RMs<-subset(specs2, species == "RM")
ggplot(data=RMs,  aes(x=wavelength, y=value, color=individual))+geom_line(linewidth=0.5)+
  scale_color_manual(values = indcolor)+facet_wrap(~ individual, scales = "free_y")

SMs<-subset(specs2, species == "SM")
ggplot(data=SMs, aes(x=wavelength, y=value, color=individual))+geom_line(linewidth=0.5)+
  scale_color_manual(values = indcolor)+facet_wrap(~ individual, scales = "free_y")

#try to get at leaf timeseries
ggplot(data=WOs, aes(x=wavelength, y=value, color=trt))+geom_line()

getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/leaf_level_spectra")
wa<-read.csv("weight.area.csv")

