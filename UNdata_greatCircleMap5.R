#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(maps)
library(geosphere)
#library(ggmap) #only if using google geolocation
require(xlsx) #only if excel file is to be read
library(RColorBrewer)
require(scales)
require(plyr)  
library(ggplot2)
library(sp)
require(rgdal)
require(reshape2)
#require(raster) #for using raster data
#source("C:/Users/Lenovo/Dropbox/Napier/R_map/GoogleHistJson/fort.R")
source("C:/Users/a1634565/Dropbox/Napier/R_map/GoogleHistJson/fort.R")
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Functions
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read and prepare data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
dataloc <- "./data/UN_MigrantStockByOriginAndDestination_2013.xls"
clist <- "./data/countriesun.xlsx"
countries <- read.xlsx2(clist, sheetName="UN",
                        colClasses=c("character",
                                     "character",
                                     "numeric",
                                     "numeric")) #list of regions, two character codes and lat-long
#function
readMigrationTable <- function(xyear = 2013) {
  #read data for a particular year #2013; 2010; 2008 & 1990
  data <- read.xlsx2(dataloc, sheetName = "Table 10", startRow = 16,
                     colIndex = c(2, 4 , 10:241),
                     colClasses = c("character", rep("numeric", 232))) #read excel sheet selected columns and rows
  #clean data
  #remove regions i.e. select only countries
  
  return(data)
}
data2013 <- readMigrationTable()
# head(data2013)
# names(data2013)
# str(data2013)
data2013 <- data2013[data2013$Country.code < 900,] #isolate countries only
# head(countries); tail(countries)
# str(countries)
countries$newname <- chartr("'", " ", countries$COUNTRY_UN)
countries$newname <- chartr("(", " ", countries$newname)
countries$newname <- chartr(")", " ", countries$newname)
countries$newname <- chartr("-", " ", countries$newname)
countries$newname <- gsub("\\s","", chartr(",", " ", countries$newname))
#convert col names to country ISCOCODEs
oldnames <- names(data2013)
newnames <- chartr(".", " ", oldnames) #replace . with space
newnames <- gsub("\\s","", newnames) #final names to match
countries$ISOCODE <- as.character(countries$ISOCODE)
#match with country codes
getCountryCode <- function(xcountry="Nepal") {
  code <- countries[countries$newname == xcountry,c("ISOCODE")]
  if (is.na(code[1])) {
    return(NA)
  } else {
    return(as.character(code[1]))
  }
}
#out <- getCountryCode("UnitedStatesVirginIslands") #returns VI
newnames2 <- sapply(newnames, getCountryCode)
newnames2[is.na(newnames2)] <- oldnames[is.na(newnames2)] #works till here :)
names(data2013) <- newnames2
data2013$newname <- chartr("'", " ", data2013[,1])
data2013$newname <- chartr("(", " ", data2013$newname)
data2013$newname <- chartr(")", " ", data2013$newname)
data2013$newname <- chartr("-", " ", data2013$newname)
data2013$newname <- gsub("\\s","", chartr(",", " ", data2013$newname))
data2013$ISOCODE <- sapply(data2013$newname, getCountryCode)
# tail(data2013,4)
#melt data
data2013.sub <- data2013[,c(-1,-2)]
# head(data2013.sub)
# str(data2013.sub)
m2013 <- melt(data2013.sub, id.vars = c("newname", "ISOCODE"),
              measure.vars = names(data2013)[3:234],
              value.name = "STOCK")
# head(m2013,12); tail(m2013)
#force na
m2013[m2013 == "NaN"] = NA
m2013[m2013 == ""] = NA
m2013 <- m2013[!is.na(m2013$STOCK),]
# str(m2013)
#force numeric
m2013$STOCK <- as.numeric(m2013$STOCK)
# m2013 <- m2013[m2013$STOCK > 0,]
#hist(log(m2013$STOCK), breaks=6) #8 bars
#map
m2013.merged <- merge(m2013, countries, by="ISOCODE", all.x=FALSE)
m2013.merged <- merge(m2013.merged, countries, by.x="variable", by.y="ISOCODE", all.x=TRUE)
m2013.merged <- m2013.merged[,c(1,2,4,6,7,10,11)]
names(m2013.merged) <- c("source", "destination", "stock", "lat.d", "lon.d", "lat.s", "lon.s")

m2013.merged$stocklog <- log(m2013.merged$stock)
#order less important first
#m2013.merged <- m2013.merged[order(m2013.merged$stock, decreasing=FALSE),]



# unique(cgc.ffm$stockcut)
#Add places
#get position of cities from http://www.geonames.org/export/ database
places <- read.delim("U:/R/Map/cities1000_old.txt", header=FALSE, sep="\t", stringsAsFactors=FALSE)
head(places)
str(places)
# ##get position of cities from naturalearth.com cities database 10m
# places2 <- readOGR(dsn="D:/R/Map/10m_populated_places", layer="ne_10m_populated_places")
# # convert to dataframe
# places2.df <- data.frame(lon=places2$LONGITUDE, lat = places2$LATITUDE)
#only keep V5 and V6 columns i.e. lat and lon
places.df <- data.frame (as.numeric(places$V6), as.numeric(places$V5), places$V9)
names(places.df) <- c('lon', "lat", "code")
# head(places.df)
# tail(places.df)
#unique(places.df$code) #178
places.df <- places.df[complete.cases(places.df),]
nrow(places.df)
str(places.df)
#summarize places for each available country
cities.count <- ddply(places.df, c("code"), function(xdf) {
  return(data.frame(count = nrow(xdf)))
})
#View(cities.count)
sum(cities.count$count) #92637
#places.df[places.df$code == "NP",]
head(m2013.merged.sub)
head(m2013.merged)
#isolate 4 countries only :) which ones to choose?
myCountries <- c("AU", "NP", "HK", "NZ", "CN", "IN", "MY", "JP", "TV", "NI", "US", "MX", "GB", "PW", "BR", "TH")
allCountries <- unique(m2013.merged$destination)
m2013.merged.sub <- m2013.merged[m2013.merged$source %in% allCountries,]
m2013.merged.sub <- m2013.merged.sub[m2013.merged.sub$stock > 0,]
m2013.merged.sub$stocklog <- round(m2013.merged.sub$stocklog,0)
nrow(m2013.merged.sub)
sum(m2013.merged.sub$stocklog)
unique(m2013.merged.sub$source)
# tail(m2013.merged.sub)
#head(m2013.merged[m2013.merged$source %in% myCountries,])
#inflate the table to incluse individual cases
m2013.merged.sub$id <- c(1:nrow(m2013.merged.sub))
m2013.merged.sub$source <- as.character(m2013.merged.sub$source)
#head(places.df)
## function gets the required locations of cities in the required country
getRandomCity <- function(xcountry = "AU", xnum=1) {
  #xcountry <- "XP"
  allCities <- places.df[places.df$code == xcountry,]
  if (nrow(allCities) == 0) {return(data.frame(lon=NA, lat=NA, code="XX"))}
  selection <- sample(c(1:nrow(allCities)), xnum, replace=TRUE)
  return(allCities[selection,])
}
##usage
getRandomCity("AE", 4) #gets 4 cities' lat long from United Arab Emirates
# replaceCities <- function(zdf) {
#   count <- nrow(zdf)
#   #source
#   for (j in 1:count) {
#     foo <- getRandomCity(zdf$source[j])
#     if (nrow(foo) == 0) {foo$lon <- 0; foo$lat <- 0}
#     bar <- getRandomCity(zdf$destination[j])
#     if (nrow(bar) == 0) {bar$lon <- 0; bar$lat <- 0}
#     zdf$lat.ss[j] <- foo$lat
#     zdf$lon.ss[j] <- foo$lon
#     zdf$lat.dd[j] <- bar$lat
#     zdf$lon.dd[j] <- bar$lon
#   }
#   return(zdf)
# }
# adf <- m2013.merged.sub[1:10,]
# getRandomCity("NP")
# head(adf)
# replaceCities(adf)
# replaceCities(data.frame(source="AU", destination="NP"))
m2013.final <- ddply(m2013.merged.sub, c("id"), function(ydf) {
  if (ydf$stocklog == 1) {data <- ydf} else {
    data <- ydf
    for (i in 2: ydf$stocklog) {
      data <- rbind(data, ydf)
    }
    return(data)
    #return(replaceCities(data))
  }
})
# tail(m2013.final); head(m2013.final)
#m2013.final2 <- replaceCities(m2013.final)
#replace cities for loop
count <- nrow(m2013.final)
zdf <- m2013.final
str(zdf)
for (j in 1:nrow(zdf)) { #nrow(zdf)
  dump <- getRandomCity(zdf$source[j])
  if (nrow(dump) == 0) {dump$lon <- 0; dump$lat <- 0}
  bar <- getRandomCity(zdf$destination[j])
  if (nrow(bar) == 0) {bar$lon <- 0; bar$lat <- 0}
  zdf$lat.s[j] <- dump$lat
  zdf$lon.s[j] <- dump$lon
  zdf$lat.d[j] <- bar$lat
  zdf$lon.d[j] <- bar$lon
}
zdf <- zdf[complete.cases(zdf),]
#is there a faster way to do this?????
#let's try matrix way!!!
# mat <- as.matrix(m2013.final$source, m2013.final$destination)
# m[m<0] <- 0
#summary(m2013.final2)
tail(m2013.final2) #n = 54613 au only, n = 7501
tail(zdf) #n = 382373
# unique(m2013.final2$source);unique(m2013.final2$destination)
unique(zdf$source);unique(zdf$destination)
head(zdf)
adf <- zdf[zdf$source %in% c("NP"),]
# max(zdf$lon.s, na.rm=T)
# zdf <- zdf[zdf$lon.s < 360,]
#zdf[zdf$lon.s == max(zdf$lon.s),]
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Process for great circle map
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#collect great circles
# geosource <- data.frame(lon=m2013.final2$lon.ss, lat=m2013.final2$lat.ss)
# geodestination <- data.frame(lon=m2013.final2$lon.dd, lat=m2013.final2$lat.dd)
geosource <- data.frame(lon=zdf$lon.s, lat=zdf$lat.s)
#head(geosource)
geodestination <- data.frame(lon=zdf$lon.d, lat=zdf$lat.d)
max(zdf$lon.s)
cgc <- gcIntermediate(geosource, geodestination, 50,
                      breakAtDateLine = TRUE, addStartEnd = TRUE, sp = TRUE)
cgc.ff <- fortify.SpatialLinesDataFrame(cgc)
#head(cgc.ff); tail(cgc.ff)
#data frame for ID
# read world shapefile from natural earth
wmap <- readOGR(dsn="U:/R/Map/110m_cultural", layer="ne_110m_admin_0_countries")
# convert to dataframe
wmap_df <- fortify(wmap)
library(RColorBrewer)
couleur <- rev(brewer.pal(52,"Blues")) #1 is dark(blue); 9 is light(white)
names(couleur) <- levels(cgc.ffm$stockcut)
colScale <- scale_colour_manual(name = "stockcut", values = couleur)

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Plot graph
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ggplot() +
  geom_polygon(aes(long,lat,group=group), 
               size = 0.2, fill="grey4", colour = NA,
               data=wmap_df) + #landmass backdrop
#   geom_point(aes(lon, lat), col="green", size=1,
#              alpha=0.01, data=places2.df) + #natural earth cities as backdrop
#   geom_point(aes(lon, lat), col="white", size=0.2,
#              alpha=0.05, data=places.df) + #geonames.org cities as backdrop
  geom_polygon(aes(long,lat,group=group), 
               size = 0.1, fill=NA, colour = "slategrey",
               data=wmap_df, alpha=0.3) + #country boundary
  #colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
    geom_line(aes(long, lat, group=group, col=order),
              data=cgc.ff, alpha = 0.02, size=0.02) + #drawing great circle lines
  #working alph = .05 size = .05
  #   geom_line(aes(lon.r,lat, color=total, 
  #                 #alpha=total,
  #                 alpha=0.8,
  #                 group=group.regroup),
  #             #col=couleur[6],
  #             size=1.1, data= cgc.ff.r) + #great circle lines overlay
  scale_colour_gradient(high="white", low="blue") +
#   colScale +
guides(alpha = "none") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_rect(fill='grey1', colour='grey1')
    ,legend.position = "none"
    ,axis.text.x  = element_blank()
    ,axis.text.y  = element_blank()
    ,axis.ticks  = element_blank()
    ,axis.title  = element_blank()
    ,axis.title  = element_blank()
  ) +   coord_equal()

#head(cgc.ffm[cgc.ffm$stock == max(cgc.ffm$stock),])
#tail(cgc.ffm); head(cgc.ffm)