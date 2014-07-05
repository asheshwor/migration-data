#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(maps)
library(geosphere)
#library(ggmap) #only if using google geolocation
require(xlsx) #only if excel file is to be read
library(RColorBrewer)
require(scales)
library(plyr)  
library(ggplot2)
library(sp)
require(rgdal)
require(reshape2)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Functions
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read and prepare data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1269.02011?OpenDocument
dataau <- "./data/AUS/standard australian classification of countries, 2011, version 2.2.xls"
clist <- "./data/countriesun.xlsx"
countries <- read.xlsx2(clist, sheetName="UN",
                        colClasses=c("character",
                                     "character",
                                     "numeric",
                                     "numeric"))
countriesau <- read.xlsx2(dataau, sheetName = "Table 4.3", startRow=10,
                          endRow=349, header=FALSE, colClasses="numeric")
countriesau <- countriesau[,c(3,4,6,7)]
head(countriesau)
names(countriesau) <- c("AUCODE", "AUNAME", "UNCODE", "UNNAME")
countriesau$AUCODE <- as.numeric(as.character(countriesau$AUCODE))
countriesau$UNCODE <- as.numeric(as.character(countriesau$UNCODE))
countriesau <- countriesau[complete.cases(countriesau),]
