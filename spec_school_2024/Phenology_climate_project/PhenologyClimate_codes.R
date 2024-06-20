install.packages("neonUtilities")
library(neonUtilities)
library(dplyr)
library(data.table)
library(phenocamapi)
library(lubridate)
library(jpeg)

####testing###########################
###test for the github pull, commit, and pull

##obtain the trait of MLBS
MLBS_trait <- loadByProduct(dpID="DP1.10026.001", 
                            site= "MLBS",
                            startdate= NA,
                            enddate= NA)
View(MLBS_trait)

##obtain the ground phenology of MLBS
# download raw data (only 23 sites have data)
MLBS_Gphenology <- loadByProduct(dpID="DP1.10055.001", 
                           site="MLBS",
                           startdate= NA,
                           enddate= NA)
View(MLBS_Gphenology)
# extract phe_perindividual and join them as a table
MLBS_perindividual <- MLBS_Gphenology$phe_perindividual
# do same process for phe_statusintensity
MLBS_intensity <- MLBS_Gphenology$phe_statusintensity
# join the phe_perindividual and phe_statusintensity based on individual ID
MLBS_combined <- left_join(MLBS_perindividual, MLBS_intensity, by = "individualID")
View(MLBS_combined)

##obtain the phenoCam data of MLBS (understory and overstory)
# obtaining the phenocam site metadata from the server as data.table
phenos <- get_phenos()

# to obtain the DB 1000 from dukehw
MLBS_under_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = 'roistats')

??get_pheno_ts
NEON.D07.MLBS.DP1.00042
NEON.D07.MLBS.DP1.00033
