#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(maps)
library(geosphere)
library(ggmap) #only if using google geolocation
require(xlsx) #only if excel file is to be read
library(RColorBrewer)
require(scales)
library(plyr) 
library(ggplot2)
library(sp)
require(rgdal)
require(descr)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read files
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
dataloc <- "C:/Users/a1634565/Dropbox/Napier/PhD/Post field/data/UN_MigrantStockByOriginAndDestination_2013.xls"
un.np.2013 <- read.xlsx(dataloc, sheetName = "Table 10", startRow = 16,
                          colIndex = c(4,154)) #read excel sheet selected columns and rows
un.np.2010 <- read.xlsx(dataloc, sheetName = "Table 7", startRow = 16,
                        colIndex = c(4,154)) #read excel sheet selected columns and rows
un.np.2000 <- read.xlsx(dataloc, sheetName = "Table 4", startRow = 16,
                        colIndex = c(4,154)) #read excel sheet selected columns and rows
un.np.1990 <- read.xlsx(dataloc, sheetName = "Table 1", startRow = 16,
                        colIndex = c(2,4,154)) #read excel sheet selected columns and rows
head(un.np.2013)
names(un.np.2013) <- c("Code","Total2013")
names(un.np.2000) <- c("Code","Total2000")
names(un.np.2010) <- c("Code","Total2010")
names(un.np.1990) <- c("Destination","Code","Total1990")
tail(un.np.2000)
head(un.np.2010)
head(un.np.2000)
tail(un.np.1990)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Merge datasets
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
temp <- merge(un.np.1990, un.np.2000, by="Code")
temp <- merge(temp, un.np.2010, by="Code")
temp <- merge(temp, un.np.2013, by="Code")
head(temp)
un.np.full <- temp
rm(temp)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Select countries
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
str(un.np.full)
un.np.cou <- un.np.full[un.np.full$Code < 900,]
un.np.cou <- un.np.cou[complete.cases(un.np.cou),]
un.np.cou <- un.np.cou[with(un.np.cou, order(-Total2013, -Total2010)), ]
head(un.np.cou)
tail(un.np.cou)
