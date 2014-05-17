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
require(reshape2)
#source("C:/Users/Lenovo/Documents/R_source/fort.R")
source("C:/Users/a1634565/Dropbox/Napier/R_map/GoogleHistJson/fort.R")
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read files
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
dataloc <- "./data/UN_MigrantStockByOriginAndDestination_2013.xls"
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
un.np.full$Destination <- as.character(un.np.full$Destination)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Select countries
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
str(un.np.full)
un.np.cou <- un.np.full[un.np.full$Code < 900,]
un.np.cou <- un.np.cou[complete.cases(un.np.cou),]
un.np.cou <- un.np.cou[with(un.np.cou, order(-Total2013, -Total2010)), ]
head(un.np.cou,20)
tail(un.np.cou)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Mapping
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
map.world <- map_data(map = "world")
str(map.world)
# how many regions
length(unique(map.world$region))
#get countires names
data.frame(unique(map.world$region))
#change some names to match world map country names
## China  -> China, Hong Kong Special Administrative Region
un.np.cou$Destination[un.np.cou$Destination == "China, Hong Kong Special Administrative Region"] <- "China"
un.np.cou$Destination[un.np.cou$Destination == "United States of America"] <- "USA"
un.np.cou$Destination[un.np.cou$Destination == "Republic of Korea"] <- "South Korea"
un.np.cou$Destination[un.np.cou$Destination == "Viet Nam"] <- "Vietnam"
un.np.cou$Destination[un.np.cou$Destination == "United Kingdom of Great Britain and Northern Ireland"] <- "UK"
names(un.np.cou) <- c("Code","region","Total1990",  "Total2000",   "Total2010",   "Total2013" )
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Merge to world map
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
## Prepare to mergeeeeeeeeeeeee
cou.list <- unique(map.world$group)
n <- length(cou.list)
new.list <- data.frame( 
  group = rep(cou.list,each=4), 
  g1 = rep(1:2, each=2,length=4*n),
  g2 = rep(1:2,length=4*n),
  value = runif(4*n)
)
head(new.list)
head(map.world)
## fill new.list with data
#er <- merge(new.list, map.world, by = "group")
mer2 <- merge(map.world, un.np.cou, by = "region", all.x=TRUE)
head(mer2)
mer2 <- mer2[order(mer2$order), ]
tail(un.np.cou)
tail(mer2)
head(mer2[mer2$region == "Australia",])
plot(mer2$Total2013)

#melt data
mer.melt <- melt(mer2, id.vars = c("region", "long", "lat", "order", "subregion", "Code"),
                 measure.vars = c("Total1990", "Total2000", "Total2010", "Total2013"),
                 value.name = "Total")
p.90 <- qplot(
  long, lat, data = mer.melt, group = group, 
  fill = Total, geom = "polygon", facets=.~variable
) + ylim(-60, 90) + facet_wrap( ~variable)
p.00 <- qplot(
  long, lat, data = mer2, group = group, 
  fill = Total2000, geom = "polygon" 
) + ylim(-60, 90)
p.10 <- qplot(
  long, lat, data = mer2, group = group, 
  fill = Total2010, geom = "polygon" 
) + ylim(-60, 90)
mapLabel <- "United Nations, Department of Economic and Social Affairs, Population Division (2013). \n Trends in International Migrant Stock: Migrants by Destination and Origin (United Nations database, POP/DB/MIG/Stock/Rev.2013)."
mapTitle <- "Trends in Nepali migrant stock population by destionation regions"
noteText <- "[ Code @ https://github.com/asheshwor/R-maps/blob/master/02_great-circle-map.R ]"

p.13 <- qplot(
  long, lat, data = mer2, group = group, 
  fill = Total2013, geom = "polygon" 
) +
  geom_polygon(aes(long,lat,group=group), 
               size = .2, fill=NA, colour = "white",
               data=map.world)
p.13 <- p.13 +  ylim(-60, 85) + guides(alpha = "none") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_rect(fill='grey24', colour='black')
    ,legend.position = c(.1,.3)
    ,legend.background = element_rect(fill = "grey24", color="darkgrey")
    ,legend.text = element_text(size = 10, colour = "mintcream")
    ,legend.title = element_text(size = 13, colour = "mintcream")
    ,axis.text.x  = element_blank()
    ,axis.text.y  = element_blank()
    ,axis.ticks  = element_blank()
    ,axis.title  = element_blank()
    ,axis.title  = element_blank()
  )

  geom_text(aes(x= 0, y=15, 
                label=mapLabel),
            color="lightgrey", size=5) +
  geom_text(aes(x= 84, y=90, 
                label=mapTitle),
            color="lightgrey", size=8) +
  geom_text(aes(x= 0, y=-60, 
                label=noteText),
            color="lightgrey", size=5) +
  coord_equal()
#plot all 4
p.13



