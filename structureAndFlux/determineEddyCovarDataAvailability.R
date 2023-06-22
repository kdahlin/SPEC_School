setwd("structureAndFlux/")
library('ncdf4')

#This is assuming you have downloaded the gap-filled CO2 flux data (eval data) from: https://data.neonscience.org/prototype-datasets/0a56e076-401e-2e0b-97d2-f986e9264a30 
filePath <- "eval/"

#Read in monthly data and concatenate into vectors
allNEE <- numeric()
allGPP <- numeric()
allTimes <- Sys.time() #Will remove the first place holder later, which was added to allow the concatenation of time data types
allSites <- character()
#siteIDs <- c("BART","GRSM","HARV","MLBS","OSBS","SCBI","SERC","TALL","TREE","UNDE") #More deciduous broadleaf NEON sites

siteIDs <- c("GRSM","MLBS","ORNL") #NEON sites within domain

for(siteID in siteIDs){
  evalFiles <- dir(path=paste0("eval/",siteID),pattern=".nc") #Identify files to loop over for each site
  for(f in seq_along(evalFiles)){
    print(evalFiles[f])
    yr_mth <- strsplit((strsplit(evalFiles[f],"_")[[1]][3]),"[.]")[[1]][1] #Determine the year and month that the file is covering
    ncDat <- nc_open(paste0(filePath,siteID,"/",evalFiles[f])) #Open nc file
    allNEE <- c(allNEE,ncvar_get(ncDat,"NEE"))
    allGPP <- c(allGPP,ncvar_get(ncDat,"GPP"))
    allTimes <- c(allTimes,ncvar_get(ncDat,"time") + 
                    as.Date(paste0(yr_mth,"-01"))) #The data is days since the first of the relevant month (measured half-hourly)
    allSites <- c(allSites,rep(siteID,length(ncvar_get(ncDat,"time"))))
    nc_close(ncDat) #Close nc file
  }
}
allTimes <- allTimes[2:length(allTimes)] #Remove the place holder time at the beginning

#Plotting 
par(mfrow=c(3,1))
for(siteID in siteIDs){
  print(siteID)
  plot(allTimes[allSites==siteID],
       allNEE[allSites==siteID],pch=20,
       xlim=range(na.omit(allTimes)),
       main=siteID,
       xlab="Time",
       ylab="NEE")
}

#MLBS July 2020 - June 2021
#GRSM 2021
#ORNL 2020, 2021 (no LAI)



# par(mfrow=c(2,1))
# plot(allTimes,allNEE,pch=20)
# plot(allTimes[allGPP>-0.1],allNEE[allGPP>-0.1],pch=20)
