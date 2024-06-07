# code for N fixer analyses. Not clearly commented :(!!
library(terra)
library(dplyr)
library(ggplot2)
library(neonUtilities)
library(spectrolab)

#setwd("F:/Postdoc/SPECSchool")
### read in Rops locations
rops<-vect("Point_ge.shp")
plot(rops)

trt<-rast("MLBS_nitrogen_mean_sd_cv_2017.tif")
plot(trt$nitrogen_mean)

buffer_rops<-buffer(rops, width=5)
rops_trt<-extract(trt, buffer_rops, fun=mean)


#random location with no Rops
norops_trt<-c()
for(i in 1:100){
  set.seed(i)
  norops<-spatSample(trt, size=17, method="random", as.points=TRUE)
  buffer_norops<-buffer(norops, width=5)
  norops_trta<-extract(trt, buffer_norops, fun=mean)
  norops_trt<-rbind(norops_trt, norops_trta) 
}

norops_trt1<-norops_trt%>%group_by(ID)%>%summarise_all(., mean, na.rm=T)

alldat<-merge(rops_trt, norops_trt1, by="ID")
colnames(alldat)<-c("ID", "Nfixer_Nmean", "Nfixer_NSD", "Nfixer_NCV", "NoNfixer_Nmean", "NoNfixer_NSD", "NoNfixer_NCV")

alldat<-reshape2::melt(alldat, id.vars="ID", variable.name= "Variable", value.name="Value" )

ggplot(subset(alldat, Variable=="Nfixer_Nmean"|Variable=="NoNfixer_Nmean"), aes(Variable, Value))+geom_boxplot()+
  theme_classic()+ylab("Leaf N")


#agb
agb<-rast("F:/Postdoc/SPECSchool/SPEC_School/N_fixer_project/Data/MLBS_agbEcoregion_20m.tif")
buffer_rops<-buffer(rops, width=50)
rops_agb<-extract(agb, buffer_rops, fun=mean)

#random location
norops_agb<-c()
for(i in 1:100){
  set.seed(i)
  norops<-spatSample(agb, size=17, method="random", as.points=TRUE)
  buffer_norops<-buffer(norops, width=50)
  norops_agba<-extract(agb, buffer_norops, fun=mean)
  norops_agb<-rbind(norops_agb, norops_agba) 
}
norops_agb1<-norops_agb%>%group_by(ID)%>%summarise_all(., mean, na.rm=T)

alldat<-merge(rops_agb, norops_agb1, by="ID")
colnames(alldat)<-c("ID", "Nfixer", "No_Nfixer")

alldat<-reshape2::melt(alldat, id.vars="ID", variable.name= "Variable", value.name="Value" )

ggplot(subset(alldat, Variable=="Nfixer"|Variable=="No_Nfixer"), aes(Variable, Value))+geom_boxplot()+
  theme_classic()+ylab("Aboveground biomass")



ndat<-data.frame(runnum, norops_agb)
#set.seed(1)
#norops_agb<-spatSample(agb, size=51, method="random")
ggplot()+geom_density(data=ndat, aes(x=MLBS_agbEcoregion_20m, fill=as.factor(runnum)), alpha=0.5)+ 
  geom_density(data=rops_agb, aes(x=MLBS_agbEcoregion_20m, fill="Rops"), alpha=0.5)


### read NEON files
#Run only once
#source("SPEC_School/N_fixer_project/NEON_data_skills/aop_merge_raster_functions.R")

#download_folder <- 'F:/Postdoc/SPECSchool'
#sp_output_folder <- 'F:/Postdoc/SPECSchool'

#makeFullSiteMosaics('DP3.30026.001','2017','MLBS',download_folder,sp_output_folder)

ndvi<-rast("2017_MLBS_2_NDVI.tif")
evi<-rast("2017_MLBS_2_EVI.tif")

buffer_rops<-terra::buffer(rops, width=5)
# Change raster accordingly
rops_1<-extract(ndvi, buffer_rops, fun=mean)

#random location
norops<-c()
for(i in 1:100){
  set.seed(i)
  norops0<-spatSample(ndvi, size=17, method="random", as.points=TRUE)
  buffer_norops<-buffer(norops, width=5)
  noropsa<-extract(ndvi, buffer_norops, fun=mean)
  norops<-rbind(norops, noropsa) 
}
norops1<-norops%>%group_by(ID)%>%summarise_all(., mean, na.rm=T)

alldat<-merge(rops_agb, norops_agb1, by="ID")
colnames(alldat)<-c("ID", "Nfixer", "No_Nfixer")

alldat<-reshape2::melt(alldat, id.vars="ID", variable.name= "Variable", value.name="Value" )

ggplot(subset(alldat, Variable=="Nfixer"|Variable=="No_Nfixer"), aes(Variable, Value))+geom_boxplot()+
  theme_classic()+ylab("Aboveground biomass")



### Plotting spectra
#Uaing package spectrolab
#setwd("F:/Postdoc/SPECSchool/SPEC_School/N_fixer_project/spectra")

rops_a<-read_spectra("mlbs_015_1.sig") # Read in black locust spectra 
maple_a<-read_spectra("mlbs_016_1.sig") # Read in red maple spectra
ndat<-data.frame(rops_a$bands, rops_a$value[,1:992],maple_a$value[,1:992])
colnames(ndat)<-c("Wavelength", "Black Locust", "Maple")

ggplot()+geom_line(data=ndat, aes(Wavelength, `Black Locust`), size=1)+
  geom_line(data=ndat, aes(Wavelength, `Maple`), col="Red", size=1)+ theme_classic()+ylab("Reflectance")+
  geom_vline(xintercept=460, linetype="dashed")+
  geom_vline(xintercept=530, linetype="dashed")+
  geom_vline(xintercept=900, linetype="dashed")+
  geom_vline(xintercept=1270, linetype="dashed")+
  geom_vline(xintercept=1730, linetype="dashed")+
  geom_vline(xintercept=1980, linetype="dashed")+
  geom_vline(xintercept=2060, linetype="dashed")+
  geom_vline(xintercept=2170, linetype="dashed")+
  geom_vline(xintercept=2240, linetype="dashed")+
  geom_vline(xintercept=2300, linetype="dashed")
  
