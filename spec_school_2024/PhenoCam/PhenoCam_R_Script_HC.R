install.packages("neonUtilities")
library(neonUtilities)
library(dplyr)
library(data.table)
library(phenocamapi)
library(lubridate)
library(jpeg)

##obtain the phenoCam data of MLBS (understory and overstory)
# obtaining the phenocam site metadata from the server as data.table
phenos <- get_phenos()

# to obtain the DB 1000 from dukehw
MLBS42_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = '3day')
View(MLBS42_UN_1000)
# date variable into date format
MLBS42_UN_1000[,date:=as.Date(date)]
# plot gcc_90
MLBS42_UN_1000[,plot(date, gcc_90, col = 'green', type = 'b')]

#> NULL

mtext('Duke Forest, Hardwood', font = 2)



MLBS_under_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = '3day')
View(MLBS_under_UN_1000)

??get_pheno_ts
NEON.D07.MLBS.DP1.00042
NEON.D07.MLBS.DP1.00033
