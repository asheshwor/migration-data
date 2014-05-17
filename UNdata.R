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
mer <- merge(new.list, map.world, by = "group")
mer2 <- merge(mer, un.np.cou, by = "region", all.x=TRUE)
tail(un.np.cou)
tail(mer2)
head(mer2[mer2$region == "Australia",])
plot(mer2$Total2013)

da <- merge(map.world, new.list,  by = "group")
qplot(
  long, lat, data = mer2, group = group, 
  fill = Total2013, geom = "polygon" 
) 



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
da <- merge(map.world, new.list,  by = "group")
qplot(
  long, lat, data = da, group = group, 
  fill = value, geom = "polygon" 
) + 
  facet_wrap( ~ g1 + g2 )


map.mig <- merge(map.world, un.np.cou, by.x = "region", by.y="region", all.x=TRUE)
tail(map.mig)
tail(map.world)
map.world2 <- map.world
map.world2$Total2013 <- 0
str(map.world2)
#list of regions
reg.list <- un.np.cou$region
value.list <- un.np.cou$Total2013
for (i in 1: length(reg.list)) {
  map.world2$Total2013[map.world2$region == reg.list[i]] <- value.list[i]
}


fill.value()
fill.value("China", 200)
#map.mig <- join(map.world, un.np.cou, by)
p <- ggplot(map.world2, aes(x = long, y = lat, group = group, fill=Total2013))
p <- p + geom_polygon(colour = "white", size = 0.3)
print(p)
str(map.world2)
unique(map.world2$Total2013)
map.mig2 <- fortify.SpatialPolygonsDataFrame(map.mig)
p3 <- ggplot(map.mig2, aes(x = long, y = lat, group = group, fill = "Total1990"))
p3 <- p3 + geom_polygon() # fill areas
p3 <- p3 + theme(legend.position="none") # remove legend with fill colours
p3 <- p3 + labs(title = "World, filled regions")

#

d1 <- map_data("state")
str(d1)
d2 <- unique(d1$group)
n <- length(d2)
d2 <- data.frame( 
  group=rep(d2,each=6), 
  g1=rep(1:3,each=2,length=6*n),
  g2=rep(1:2,length=6*n),
  value=runif(6*n)
)
head(d2)
head(d1)
tail(d2)
tail(d1)
d <- merge(d1, d2,  by="group")
qplot(
  long, lat, data = d, group = group, 
  fill = value, geom = "polygon" 
) + 
  facet_wrap( ~ g1 + g2 )