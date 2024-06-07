setwd("structureAndFlux/")
library('ncdf4')
library('tidyverse')

#This is assuming you have downloaded the gap-filled CO2 flux data (eval data) from: https://data.neonscience.org/prototype-datasets/0a56e076-401e-2e0b-97d2-f986e9264a30 
filePath <- "eval/"

#Read in monthly data and concatenate into vectors
allNEE <- numeric()
allGPP <- numeric()
allTimes <- Sys.time() #Will remove the first place holder later, which was added to allow the concatenation of time data types
allSites <- character()
#siteIDs <- c("BART","GRSM","HARV","MLBS","OSBS","SCBI","SERC","TALL","TREE","UNDE") #More deciduous broadleaf NEON sites

siteIDs <- c("GRSM","MLBS","ORNL") #NEON sites within domain
f=32
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

allResp <- allNEE + allGPP

#Plotting
#Full Time-series for MLBS
siteID <- "MLBS"
print(siteID)
jpeg(file="MLBS_flux.jpg",
     height=8,width=12,units="in",res=1000)

par(mfrow=c(3,1),mai=c(0.7,1.2,0.7,1))

plot(allTimes[allSites==siteID],
     allNEE[allSites==siteID],pch=20,
     xlim=range(na.omit(allTimes)),
     main=paste(siteID,"Net Ecosystem Exchange"),
     xlab="Time",
     ylab="NEE (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(allTimes[allSites==siteID],
     allGPP[allSites==siteID],pch=20,
     xlim=range(na.omit(allTimes)),
     main=paste(siteID,"Gross Primary Productivity"),
     xlab="Time",
     ylab="GPP (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(allTimes[allSites==siteID],
     allResp[allSites==siteID],pch=20,
     xlim=range(na.omit(allTimes)),
     main=paste(siteID,"Ecosystem Respiration"),
     xlab="Time",
     ylab="R (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

dev.off()

#Month of AOP Fly Over (June 2021)
siteID <- "MLBS"
print(siteID)
jpeg(file="MLBS_flux_June2021.jpg",
     height=8,width=12,units="in",res=1000)

par(mfrow=c(3,1),mai=c(0.7,1.2,0.7,1))

plot(allTimes[allSites==siteID & 
                allTimes>as.POSIXct("2021-06-01") &
                allTimes<as.POSIXct("2021-07-01")],
     allNEE[allSites==siteID & 
              allTimes>as.POSIXct("2021-06-01") &
              allTimes<as.POSIXct("2021-07-01")],
     type="l",
     main=paste(siteID,"Net Ecosystem Exchange"),
     xlab="Time",
     ylab="NEE (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(allTimes[allSites==siteID & 
                allTimes>as.POSIXct("2021-06-01") &
                allTimes<as.POSIXct("2021-07-01")],
     allGPP[allSites==siteID & 
              allTimes>as.POSIXct("2021-06-01") &
              allTimes<as.POSIXct("2021-07-01")],
     type="l",
     main=paste(siteID,"Gross Primary Productivity"),
     xlab="Time",
     ylab="GPP (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(allTimes[allSites==siteID & 
                allTimes>as.POSIXct("2021-06-01") &
                allTimes<as.POSIXct("2021-07-01")],
     allResp[allSites==siteID & 
               allTimes>as.POSIXct("2021-06-01") &
               allTimes<as.POSIXct("2021-07-01")],
     type="l",
     main=paste(siteID,"Ecosystem Respiration"),
     xlab="Time",
     ylab="R (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

dev.off()

#Calculate Daily Values
combinedData <- data.frame(timeVal=allTimes,
                           NEE=allNEE,
                           GPP=allGPP,
                           Resp=allResp,
                           siteID=allSites)
dailyData <- combinedData %>% 
  mutate(day=lubridate::date(timeVal)) %>%
  group_by(siteID,day) %>%
  summarise(meanNEE=mean(NEE,na.rm=TRUE),
            meanGPP=mean(GPP,na.rm=TRUE),
            meanR=mean(Resp,na.rm=TRUE))

#Plot Daily Data
jpeg(file="MLBS_flux_daily.jpg",
     height=8,width=12,units="in",res=1000)

par(mfrow=c(3,1),mai=c(0.7,1.2,0.7,1))

plot(dailyData$day[dailyData$siteID==siteID],
     dailyData$meanNEE[dailyData$siteID==siteID],
     type="l",
     xlim=range(na.omit(dailyData$day)),
     main=paste(siteID,"Daily Mean Net Ecosystem Exchange"),
     xlab="Time",
     ylab="NEE (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(dailyData$day[dailyData$siteID==siteID],
     dailyData$meanGPP[dailyData$siteID==siteID],
     type="l",
     xlim=range(na.omit(dailyData$day)),
     main=paste(siteID,"Daily Mean Gross Primary Productivity"),
     xlab="Time",
     ylab="GPP (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

plot(dailyData$day[dailyData$siteID==siteID],
     dailyData$meanR[dailyData$siteID==siteID],
     type="l",
     xlim=range(na.omit(dailyData$day)),
     main=paste(siteID,"Daily Mean Ecosystem Respiration"),
     xlab="Time",
     ylab="R (umol m-2 s-1)",bty="n",cex.lab=2,cex.axis=2,cex.main=2)
abline(h=0,col="red",lwd=2)

dev.off()

#Determine Means
siteNEE <- na.omit(allNEE[allSites==siteID])
siteGPP <- na.omit(allGPP[allSites==siteID])
siteR <- na.omit(allResp[allSites==siteID])

allNEEmeans <- numeric()
allGPPmeans <- numeric()
allRmeans <- numeric()

for(i in 1:5000){ #Bootstrapping to determine uncertainties in means
  randomIs <- sample(1:length(siteNEE),10000,replace=T)
  allNEEmeans <- c(allNEEmeans,mean(siteNEE[randomIs]))
  allGPPmeans <- c(allGPPmeans,mean(siteGPP[randomIs]))
  allRmeans <- c(allRmeans,mean(siteR[randomIs]))
}
mean(allNEE[allSites==siteID],na.rm=TRUE)
sd(allNEEmeans)

mean(allGPP[allSites==siteID],na.rm=TRUE)
sd(allGPPmeans)

mean(allResp[allSites==siteID],na.rm=TRUE)
sd(allRmeans)


#MLBS July 2020 - June 2021 (June 2021 for AOP)
#GRSM 2021
#ORNL 2020, 2021 (no LAI)

