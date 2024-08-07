
#Jahiya Clark 
##Source: https://www.neonscience.org/resources/learning-hub/tutorials/phenocam-api-intro

install.packages("neonUtilities")

# Install thePhenocampi package from the GitHub repo
#if(!require(devtools)) install.packages('devtools')
#devtools::install_github('bnasr/phenocamapi')

# loading the package
library(phenocamapi)
library(ggplot2)
library(dplyr)
library(neonUtilities)
library(dplyr)
library(data.table)
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

under <- as_tibble(MLBS42_UN_1000)

under %>%
  mutate(year=as.factor(year)) %>%
ggplot(aes(x=doy,y=gcc_90,color=year,group=year)) +
  geom_line() +
  theme_minimal()

over <- as_tibble(MLBS33_DB_2000)

over %>%
  mutate(year=as.factor(year)) %>%
  ggplot(aes(x=doy,y=gcc_90,color=year,group=year)) +
  geom_line() +
  theme_minimal()

# plot over & understory in same year
MLBS33_DB_2000$label = 'overstory'
MLBS42_UN_1000$label = 'understory'
dat <- rbind(MLBS33_DB_2000, MLBS42_UN_1000)

ggplot(data=dat %>% filter(year>2017)) +
  geom_point(aes(x=date, y=gcc_90, color=label)) +
  facet_wrap(~year, scales='free_x')
