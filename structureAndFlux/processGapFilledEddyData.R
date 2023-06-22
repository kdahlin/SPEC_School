setwd("structureAndFlux/")
library('ncdf4')

#This is assuming you have downloaded the gap-filled CO2 flux data (eval data) from: https://data.neonscience.org/prototype-datasets/0a56e076-401e-2e0b-97d2-f986e9264a30 
filePath <- "eval/"

#Read in monthly data and concatenate into vectors
allNEE <- numeric()
allGPP <- numeric()
allTimes <- Sys.Date()
siteID <- "MLBS"
for(mth in c("07","08","09","10","11","12")){
  print(mth)
  ncDat <- nc_open(paste0(filePath,siteID,"/",siteID,"_eval_2020-",mth,".nc"))
  allNEE <- c(allNEE,ncvar_get(ncDat,"NEE"))
  allGPP <- c(allGPP,ncvar_get(ncDat,"GPP"))
  allTimes <- c(allTimes,ncvar_get(ncDat,"time") + 
                  as.Date(paste0("2020-",mth,"-01")))
  nc_close(ncDat)
}
allTimes <- allTimes[2:length(allTimes)]

for(mth in c("01","02","03","04","05","06")){
  print(mth)
  ncDat <- nc_open(paste0(filePath,siteID,"/",siteID,"_eval_2021-",mth,".nc"))
  allNEE <- c(allNEE,ncvar_get(ncDat,"NEE"))
  allGPP <- c(allGPP,ncvar_get(ncDat,"GPP"))
  allTimes <- c(allTimes,ncvar_get(ncDat,"time") + 
                  as.Date(paste0("2021-",mth,"-01")))
  nc_close(ncDat)
}

plot(allTimes,allGPP,pch=20)

par(mfrow=c(2,1))
plot(allTimes,allNEE,pch=20)
plot(allTimes[allGPP>-0.1],allNEE[allGPP>-0.1],pch=20)
