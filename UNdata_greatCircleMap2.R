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
#require(raster) #for using raster data
source("C:/Users/a1634565/Dropbox/Napier/R_map/GoogleHistJson/fort.R")
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Functions
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# the functions to regroup, and close polygons have been used from
# https://github.com/cengel/GreatCircle/blob/master/GreatCircleFlights.R
# as explained here, 
# http://www.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}
### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}
#
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read and prepare data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
dataloc <- "./data/UN_MigrantStockByOriginAndDestination_2013.xls"
clist <- "./data/countriesun.xlsx"
countries <- read.xlsx(clist, sheetName="UN") #list of regions, two character codes and lat-long
#function
readMigrationTable <- function(xyear = 2013) {
  #read data for a particular year #2013; 2010; 2008 & 1990
  data <- read.xlsx2(dataloc, sheetName = "Table 10", startRow = 16,
             colIndex = c(2, 4 , 10:241)) #read excel sheet selected columns and rows
  #clean data
  #remove regions i.e. select only countries
  
  return(data)
}

data2013 <- readMigrationTable()
head(data2013)
names(data2013)
head(countries); tail(countries)
countries$newname <- gsub("\\s","", chartr(",", " ", countries$COUNTRY_UN))
#convert col names to country ISCOCODEs
oldnames <- names(data2013)
newnames <- chartr(".", " ", oldnames) #replace . with space
newnames <- gsub("\\s","", newnames) #final names to match


# x <- "http://stat.umn.edu:80/xyz"
# m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
# m
# regmatches(x, m)
newnames
#read data for country  - countryName
readMigrationData <- function(cindex) {
  #find row for the country
  index <- cindex + 10 #first column in J
  un.np.2013 <- read.xlsx2(dataloc, sheetName = "Table 10", startRow = 16,
                          colIndex = c(4,index)) #read excel sheet selected columns and rows
  un.np.2010 <- read.xlsx2(dataloc, sheetName = "Table 7", startRow = 16,
                          colIndex = c(4,index)) #read excel sheet selected columns and rows
  un.np.2000 <- read.xlsx2(dataloc, sheetName = "Table 4", startRow = 16,
                          colIndex = c(4,index)) #read excel sheet selected columns and rows
  un.np.1990 <- read.xlsx2(dataloc, sheetName = "Table 1", startRow = 16,
                          colIndex = c(2,4,index)) #read excel sheet selected columns and rows
  names(un.np.2013) <- c("Code","Total2013")
  names(un.np.2000) <- c("Code","Total2000")
  names(un.np.2010) <- c("Code","Total2010")
  names(un.np.1990) <- c("Destination","Code","Total1990")
  temp <- merge(un.np.1990, un.np.2000, by="Code")
  temp <- merge(temp, un.np.2010, by="Code")
  temp <- merge(temp, un.np.2013, by="Code")
  un.np.full <- temp
  #rm(list = c("temp", "un.np.1990", "un.np.2000", "un.np.2010", "un.np.2013"))
  un.np.full$Destination <- as.character(un.np.full$Destination)
  #str(un.np.full)
  un.np.cou <- un.np.full[un.np.full$Code < 900,] #isolate countries only
  un.np.cou <- un.np.cou[complete.cases(un.np.cou),] #isolate countires with no
  ##    data on migration stock from the specific country
  un.np.cou <- un.np.cou[with(un.np.cou, order(-Total2013, -Total2010)), ] #sort table
  #tail(un.np.cou)
  #add column on isocode for countires
  un.np.cou <- merge(un.np.cou, countries, by.x = "Destination", by.y = "COUNTRY_UN")
  return(un.np.cou)
}
adf <- readMigrationData(2)
head(adf)
tail(adf)
#rm(list=ls())

#collect arcs for each country
xdata <- NULL
for (i in 1:2) {
  xdata <- rbind(xdata, readMigrationData(i))
}

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Mapping
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#the origin point of the lines
p1 <- c(84.12, 28.39) #center of Nepal, sort of
countries <- adf
head(countries)
head(un.np.cou)
#loc.geo <- data.frame(lon = as.numeric(un.np.cou$LON), lat = as.numeric(un.np.cou$LAT))
loc.geo <- data.frame(lon = as.numeric(adf$LON), lat = as.numeric(adf$LAT))
#define colours
#couleur1 <- brewer.pal(12, "Paired")
couleur <- brewer.pal(9, "PuRd")
# read world shapefile from natural earth
wmap <- readOGR(dsn="./data/world", layer="ne_10m_admin_0_countries") #read shape file
# convert to dataframe
wmap_df <- fortify(wmap)
#collect great circles from p1 to each country
cgc <- gcIntermediate(p1, loc.geo, 100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
cgc.ff <- fortify.SpatialLinesDataFrame(cgc)
#data frame for id
loc.geo.df <- data.frame(loc.geo) # debug alert
loc.geo.df$location <- adf$ISOCODE
loc.geo.df$total <- adf$Total2013
loc.geo.df$id <- as.character(c(1:nrow(loc.geo.df))) #making id character
names(loc.geo.df) <- c("lon.old", "lat.old", "location", "total", "id")
cgc.ffm <- merge(cgc.ff, loc.geo.df, all.x=T, by="id")
#move map center to Nepal
center <- 84
# shift coordinates to recenter great circles
cgc.ffm$lon.r <- ifelse(cgc.ffm$long < center -180, cgc.ffm$long +360, cgc.ffm$long)
# shift coordinates to recenter worldmap
# worldmap <- map_data ("world")
# worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)
wmap_df$long.recenter <- ifelse(wmap_df$long  < center - 180 , wmap_df$long + 360, wmap_df$long)
# now regroup
cgc.ff.r <- ddply(cgc.ffm, .(id), RegroupElements, "lon.r", "id")
#worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
worldmap.rg <- ddply(wmap_df, .(group), RegroupElements, "long.recenter", "group")
# close polys
#worldmap.cp <- worldmap.rg
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Plotting using ggplot2
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
mapLabel <- "Permit figures from Department of Foreign Employment www.dofe.gov.np \n Country boundaries from NaturalEarthData.com \n Location of cities from NaturalEarthData.com & geonames.org"
mapTitle <- "Nepalis working abroad - international work permits issued and destination countries FY 2067-68 (2010-11)"
noteText <- "[ Code @ https://github.com/asheshwor/R-maps/blob/master/02_great-circle-map.R ]"
ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
               size = 0.2, fill="rosybrown", colour = NA,
               data=worldmap.cp) + #country backdrop
    geom_polygon(aes(long.recenter,lat,group=group.regroup), 
               size = 0.1, fill=NA, colour = "indianred4",
               data=worldmap.cp, alpha=0.5) + #country boundary
  geom_line(aes(lon.r, lat, group=group.regroup),
            col=couleur[4],
            size=.3, alpha=0.7, data= cgc.ff.r) + #drawing great circle lines
  geom_line(aes(lon.r,lat, color=total, 
                #alpha=total,
                alpha=0.8,
                group=group.regroup),
            #col=couleur[6],
            size=1.1, data= cgc.ff.r) + #great circle lines overlay
  guides(alpha = "none") +
  scale_colour_gradient(high="red1", low="navy", 
                        trans = "log",
                        name="Permits",
                        labels = comma,
                        #breaks=seq(min(np.data$Total), max(np.data$Total), by=1000)
                        breaks=c(2, 10, 100, 1000, 10000, 100000)
  ) +
  ylim(-60, 90) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_rect(fill='skyblue2', colour='black')
    ,legend.position = c(.9,.3)
    ,legend.background = element_rect(fill = "skyblue2", color="darkgrey")
    ,legend.text = element_text(size = 10, colour = "gray0")
    ,legend.title = element_text(size = 13, colour = "gray0")
    ,axis.text.x  = element_blank()
    ,axis.text.y  = element_blank()
    ,axis.ticks  = element_blank()
    ,axis.title  = element_blank()
    ,axis.title  = element_blank()
  ) + 
  geom_text(aes(x= 200, y=-56, 
                label=mapLabel),
            color="gray0", size=5) +
  geom_text(aes(x= 84, y=90, 
                label=mapTitle),
            color="gray0", size=8) +
  geom_text(aes(x= 0, y=-60, 
                label=noteText),
            color="gray0", size=5) +
  coord_equal()
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Merge to world map
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
## printing using Natural earth world map
wmap <- readOGR(dsn="./data/world", layer="ne_10m_admin_0_countries") #read shape file
#world_adm0.dbf
wmap@data$id <- rownames(wmap@data) #add id column as rownames
wmap.df <- fortify(wmap) #convert to dataframe
#head(wmap.df,3)
#tail(wmap.df,4)
wmap.df <- join(wmap.df, wmap@data, by = "id")
wmap.df <- wmap.df[order(wmap.df$order), ] #may not be necessary ***check***
#str(un.np.cou)
wmap.df <- merge(wmap.df, un.np.cou, by.x="ISO_A2", by.y="ISOCODE", all.x=T, sort=F) #merge data
wmap.df <- wmap.df[order(wmap.df$order), ]
#str(wmap.df)
#head(un.np.cou)
wmap.df <- wmap.df[c("ISO_A2", "long", "lat", "order", "hole", "piece", "group", "id", "ADMIN",
                     "SOVEREIGNT", "Total1990", "Total2000", "Total2010", "Total2013")]
wmap.melt <- melt(wmap.df, id.vars = c("ISO_A2", "ADMIN", "long", "lat", "order", "hole",
                                       "piece", "group", "id"),
                  measure.vars = c("Total1990", "Total2000", "Total2010", "Total2013"),
                  value.name = "Total")
head(wmap.melt,50)
tail(wmap.melt)
wmap.melt <- wmap.melt[order(wmap.melt$order), ]
#analyze numbers
plot(wmap.melt[wmap.melt$Total < 1000000, "Total"])
p.all <- qplot(
  long, lat, data = wmap.melt, group = group, 
  fill = Total, geom = "polygon", facets=.~variable) +
  scale_fill_gradient2(high ="red1", mid = "green3",
                       labels = comma, trans="log",
                       breaks=c(100, 500, 2000, 10000, 40000, 100000, 500000)) +
  ylim(-60, 90) + facet_wrap( ~variable, ncol=2)
p.all + geom_path(color="white", linestyle=2, size=.1, alpha=0.5) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_rect(fill='skyblue1', colour='black')
    ,legend.position = c(.04,.11)
    ,legend.background = element_rect(fill = "darkgray", color="darkgrey")
    ,legend.text = element_text(size = 10, colour = "mintcream")
    ,legend.title = element_text(size = 13, colour = "mintcream")
    ,axis.text.x  = element_blank()
    ,axis.text.y  = element_blank()
    ,axis.ticks  = element_blank()
    ,axis.title  = element_blank()
    ,axis.title  = element_blank()
  )
###end
map.plot <- ggplot(data=wmap.df, aes(x=long, y=lat, group=group))
map.plot <- map.plot + geom_polygon(aes(fill=Total2013))
map.plot <- map.plot + geom_path(color="gray", linestyle=2)
map.plot <- map.plot + coord_equal() 
map.plot <- map.plot  + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                                            space = "Lab", na.value = "grey50",
                                            guide = "colourbar")
map.plot <- map.plot  + labs(title="Migration 2013")
print(map.plot)

#facet plot
#melt data
head(wmap.df)
names(wmap.df)

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Practice code
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

