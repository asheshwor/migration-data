places <- read.delim("U:/R/Map/cities1000.txt", header=FALSE, sep="\t")
head(places)
tail(places)
places[c(140:150),c("V2","V9")]

#only keep V5, V6 and V9 columns i.e. lat and lon
places.df <- data.frame (places$V6, places$V5, places$V9)
names(places.df) <- c('lon', "lat", "code")
head(places.df)
tail(places.df)
places.df[sample(1:nrow(places.df), 10),]
unique(places.df$code)
head(m2013.merged)
summary(m2013.merged)
sum(m2013.merged$stock) #225,012,650
