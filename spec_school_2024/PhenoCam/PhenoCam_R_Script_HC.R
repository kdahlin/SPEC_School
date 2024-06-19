##Source: https://www.neonscience.org/resources/learning-hub/tutorials/phenocam-api-intro

install.packages("neonUtilities")
install.packages("ggplot2")
install.packages("dplyr") # For data manipulation
library(ggplot2)
library(dplyr)
library(neonUtilities)
library(dplyr)
library(data.table)
library(phenocamapi)
library(lubridate)
library(jpeg)

##obtain the phenoCam data of MLBS (understory and overstory)
# obtaining the phenocam site metadata from the server as data.table
phenos <- get_phenos()

# to obtain the UN 1000 from understory MLBS (https://phenocam.nau.edu/webcam/roi/NEON.D07.MLBS.DP1.00042/UN_1000/)
MLBS42_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = '3day')
View(MLBS42_UN_1000)
# date variable into date format
MLBS42_UN_1000[,date:=as.Date(date)]
# plot gcc_90 (vegetation indices)
MLBS42_UN_1000[,plot(date, gcc_90, col = 'green', type = 'b')]

# to obtain the DB 2000 from overstory MLBS (https://phenocam.nau.edu/webcam/roi/NEON.D07.MLBS.DP1.00033/DB_2000/)
MLBS33_DB_2000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00033', vegType = 'DB', roiID = 2000, type = '3day')
View(MLBS33_DB_2000)
# date variable into date format
MLBS33_DB_2000[,date:=as.Date(date)]
# plot gcc_90 (vegetation indices)
MLBS33_DB_2000[,plot(date, gcc_90, col = 'green', type = 'b')]
