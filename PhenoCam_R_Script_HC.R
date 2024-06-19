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
MLBS_under_UN_1000 <- get_pheno_ts(site = 'NEON.D07.MLBS.DP1.00042', vegType = 'UN', roiID = 1000, type = 'roistats')

??get_pheno_ts
NEON.D07.MLBS.DP1.00042
NEON.D07.MLBS.DP1.00033
