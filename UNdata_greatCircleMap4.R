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
# head(m2013.merged); tail(m2013.merged)
# hist(m2013.merged$stock)
# plot(m2013.merged$stocklog)
#m2013.merged <- m2013.merged[m2013.merged$stock > 0,]
m2013.merged$stocklog <- log(m2013.merged$stock)
#order less important first
#m2013.merged <- m2013.merged[order(m2013.merged$stock, decreasing=FALSE),]


#collect great circles
geosource <- data.frame(lon=m2013.merged$lon.s, lat=m2013.merged$lat.s)
geodestination <- data.frame(lon=m2013.merged$lon.d, lat=m2013.merged$lat.d)
selection <- c(1:2577, 2579:5400, 5500:5850, 6000:13613)
cgc <- gcIntermediate(geosource[selection,], geodestination[selection,], 100,
                      breakAtDateLine = TRUE, addStartEnd = TRUE, sp = TRUE)
# selection2 <- 5000:5400
# cgcx <- gcIntermediate(geosource[selection2,], geodestination[selection2,],
#                        100, breakAtDateLine = TRUE, addStartEnd = TRUE,
#                        sp = TRUE)
#max(geosource$lon); max(geosource$lat);max(geodestination$lon); max(geodestination$lat);
#min(geosource$lon); min(geosource$lat);min(geodestination$lon); min(geodestination$lat);
#geosource[2570:2580,];geodestination[2570:2580,]
cgc.ff <- fortify.SpatialLinesDataFrame(cgc)
head(cgc.ff); tail(cgc.ff)
#data frame for ID
geo.df <- m2013.merged
geo.df$id <- as.character(c(1:nrow(geo.df)))
# head(geo.df)
cgc.ffm <- merge(cgc.ff, geo.df, all.x=TRUE, by="id")
#cgc.ff.r <- ddply(cgc.ffm, .(id), RegroupElements, "lon.r", "id")
##ggplot2
##prepare background
# read world shapefile from natural earth
wmap <- readOGR(dsn="U:/R/Map/110m_cultural", layer="ne_110m_admin_0_countries")
# convert to dataframe
wmap_df <- fortify(wmap)

# tail(cgc.ffm)
# plot(cgc.ffm$stocklog)
# hist(cgc.ffm$stocklog)
# plot(cgc.ffm$stock)
# hist(log(cgc.ffm$stock), breaks=1000)
#remove those with stock = 0
# min(cgc.ffm$stocklog)
# head(cgc.ffm[cgc.ffm$stocklog == -Inf,])
# str(cgc.ffm)
#cgc.ffm <- cgc.ffm[cgc.ffm$stock > 0,]
cgc.ffm <- within(cgc.ffm, stocklog[stocklog == -Inf] <- 0)
#cut
cgc.ffm$stockcut <- cut(cgc.ffm$stocklog, breaks=9, labels=FALSE,
                        ordered_result = TRUE)
# plot(cgc.ffm$stockcut)
# hist(cgc.ffm$stockcut)
#cgc.ffm$alpha <- 0.01 + cgc.ffm$stockcut/500
#hist(cgc.ffm$alpha)
#head(cgc.ffm)
tail(cgc.ffm)
#cgc.ffm[c(1000,500,25,36,10000),]
#cgc.ffm <- cgc.ffm[order(cgc.ffm$stock, decreasing=FALSE),]
# 
# cgc.ffm2 <- cgc.ffm[1:50000,]
# cgc.ffm2 <- cgc.ffm2[order(cgc.ffm2$stockcut),]
#colour
# pal <- colorRampPalette(c("#f2f2f2", "red"))
# colors <- pal(100)
# colors[90]
library(RColorBrewer)
couleur <- rev(brewer.pal(9,"Blues")) #1 is dark(blue); 9 is light(white)
cgc.ffm$stockcut <- factor(cgc.ffm$stockcut)
names(couleur) <- levels(cgc.ffm$stockcut)
colScale <- scale_colour_manual(name = "stockcut", values = couleur)
# unique(cgc.ffm$stockcut)



ggplot() +
  geom_polygon(aes(long,lat,group=group), 
               size = 0.2, fill="grey4", colour = NA,
               data=wmap_df) + #landmass backdrop
  #   geom_point(aes(lon, lat), col=couleur[2], size=0.5,
  #              alpha=0.01, data=places2.df) + #natural earth cities as backdrop
  #   geom_point(aes(lon, lat), col=couleur[2], size=0.2,
  #              alpha=0.01, data=places.df) + #geonames.org cities as backdrop
  geom_polygon(aes(long,lat,group=group), 
               size = 0.1, fill=NA, colour = "slategrey",
               data=wmap_df, alpha=0.3) + #country boundary
  #colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
  geom_line(aes(long, lat, group=group, col=stockcut),
            data=cgc.ffm,
            alpha = .05, size=0.1) + #drawing great circle lines
  #   geom_line(aes(lon.r,lat, color=total, 
  #                 #alpha=total,
  #                 alpha=0.8,
  #                 group=group.regroup),
  #             #col=couleur[6],
  #             size=1.1, data= cgc.ff.r) + #great circle lines overlay
  #scale_colour_gradient(high="white", low="blue") +
  colScale +
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
