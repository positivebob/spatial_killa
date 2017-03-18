##### ____ Robert McGuinn startdate_20151017 enddate... #####

##### install packages #####
# install.packages("sp")
# install.packages("rgdal")
# install.packages("maptools") 
# install.packages("ggplot2")
# install.packages("rgeos") 
# install.packages("gcclib") 
# install.packages("ggmap") 
# install.packages("dplyr")
# install.packages("lazyeval")

library(sp)
library(rgdal)
library(maptools)
library(ggplot2)
library(rgeos)
library(ggmap)
library(dplyr)
library(lazyeval)

##### ____ Mapping the McGuinn land in Polk County, NC #####

##### various working directories #####
setwd("C:/rworking")
setwd("C:/rworking/spatial_killa/code")
setwd("C:/rworking/spatial_killa/inputs")
setwd("C:/rworking/spatial_killa/outputs")
setwd("C:/rworking/spatial_killa/plots")

##### read in input files #####

# set working directory to polkcounty
setwd("C:/rworking/spatial_killa/inputs/polkcounty")

# read in the shapefile using R's interface to the Geospatial Data Abstraction Library (rGDAL)
# load the shapefile and turn it into a SpatialPolygonDataFrame
library(rgdal)
land <- readOGR(dsn = ".", "mcguinnland")

# print the contents to see the structure of the file
print(land)

# look at the variables in the SpatialPolygonDataFrame
names(land)

# look at the attribute table in the slot called data
land@data

# look at the projection, it is Lambert Conic Conformal (1SP)
proj4string(land)

# do a summary of land
summary(land)

# checking ownership
unique(land$OWNAM1)
unique(land$OWNAM3)

# selecting parts of the dataframe for plotting
p <- ggplot(land@data, aes(OWNAM1, LAND_VALUE))
p <- ggplot(land@data, aes(OWNAM1, area))
p <- ggplot(land@data, aes(OWNAM1, TMS))
p <- ggplot(land@data, aes(OWNAM1, DEED_YEAR))
p <- ggplot(land@data, aes(OWNAM1, DEEDED_ACR))

# adding a geometry to the plot
p + geom_point(colour= "black", size=6)
p + geom_point(colour=land@data$DEED_YEAR, size=6) + geom_text(colour = "red", size = 5, aes(label = land@data$LAND_VALUE))
p + geom_point(colour=land@data$DEED_YEAR, size=6) + geom_text(colour = "red", size = 5, aes(label = land@data$LAND_VALUE))


# make the polygons plottable with ggplot2 using fortify
land.f <- fortify(land, region = "TMS")

# add back the attribute data to the polygons
land.f <- merge(land.f, land@data, by.x = "id", by.y = "TMS")

# check the object
print(land.f)

# make the map
map <- ggplot(land.f, aes(long, lat, group = group, fill = LAND_VALUE)) + 
  geom_polygon() + 
  coord_equal() + 
  labs(x = "Easting (feet)", y = "Northing (feet)", fill = "Land Value") + 
  ggtitle("McGuinn Land")

# plot the map 
map

# save the map
ggsave("my_large_plot.png", scale = 1, dpi = 400)

# get a wgs84 version of the original SpatialPolygonDataFrame
land.wgs84 <- spTransform(land, CRS("+init=epsg:4326"))

# fortify the wgs version of the land polys for plotting purposes
land.wgs84.f <- fortify(land.wgs84, region = "TMS")
land.wgs84.f <- merge(land.wgs84.f, land.wgs84@data, 
                      by.x = "id", by.y = "TMS")

##### Extracting coordinates from KML file #####
#Extracting Coordinates and ID from KML  
setwd("C:/rworking/spatial_killa/inputs")
kml.text <- readLines("point.kml")  

re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
coords <- grep(re,kml.text)  

re2 <- "src_id:"  
SCR.ID <- grep(re2,kml.text)  

re3 <- "<tr><td><b>Name:</b><td>"  
Name <- grep(re3,kml.text)  

kml.coordinates <- matrix(0,length(coords),4,dimnames=list(c(),c("ID","LAT","LON","ELEV")))  
kml.names <- matrix(0,length(coords),1)  

for(i in 1:length(coords)){  
  sub.coords <- coords[i]  
  temp1 <- gsub("<coordinates>"," ",kml.text[sub.coords])  
  temp2 <- gsub("</coordinates>"," ",temp1)  
  coordinates <- as.numeric(unlist(strsplit(temp2,",")))  
  
  sub.ID <- SCR.ID[i]  
  ID <- as.numeric(gsub("<tr><td><b>src_id:</b><td>"," ",kml.text[sub.ID]))  
  
  sub.Name <- Name[i]  
  NAME <- gsub(paste("<tr><td><b>Name:</b><td>"),"",kml.text[sub.Name])  
  
  kml.coordinates[i,] <- matrix(c(ID,coordinates),ncol=4)  
  kml.names[i,] <- matrix(c(NAME),ncol=1)  
}  


write.table(kml.coordinates,"KML_coordinates.csv",sep=";",row.names=F)

##### Getting a google map from KML coordinates(see above on how to get coordinates) #####
google <- get_googlemap(c(kml.coordinates[1,2],kml.coordinates[1,3]), 
                        zoom = 14,
                        maptype = "satellite")

ggmap(google) +
  geom_point(aes(x = c(kml.coordinates[1,2]), y = c(kml.coordinates[1,3]), colour = "red", size = 10))

##### making the map #####
# create a bounding box we will use to get base map
b <- bbox(land.wgs84) 


# get a basemap using bbox and ggmap 
## hybrid map
lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "hybrid", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  LAND_VALUE), 
               alpha = 0.5)

## terrain map

lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "terrain", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  LAND_VALUE, 
                   alpha = 0.7))

## Sattellite Map

lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "satellite", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  LAND_VALUE), 
               alpha = 0.7)

                        

##### ____ Using ggmap #####

##### Getting a map #####
# set location variable
location <- "Gladwin, Michigan"
gc <- geocode(location)

# customizing gc

# gc
# gc$lon <- kml.coordinates[1,3]
# gc$lat <- kml.coordinates[1,2]
  

google <- get_googlemap(c(gc$lon,gc$lat), 
                        zoom = 13,
                        maptype = "hybrid")


lon
ggmap(google) +
  geom_point(aes(x = lon, y = lat), data=gc, colour = "red", size = 4)

#### bounding box creation ####
# bbox creation
z <- .45
#bbox <- c(left = gc$lon-.01, bottom = gc$lat-.01, right = gc$lon+.01, top = gc$lat+.01)
bbox <- c(left = gc$lon-z, bottom = gc$lat-z, right = gc$lon+z, top = gc$lat+z)

##### ____ Working with searching Google maps API (from user) ####
coordenadas<-geocode(location)
encontrar<-function(lugar,radius,keyword){
  
  # radius in meters
  # lugar is coordinates from google maps by hand
  coor<-paste(lugar[1],lugar[2],sep=",")
  baseurl<-"https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
  google_key<-c("AIzaSyCW0iITfCmFQMHxp8vEd4c4c9r1rDDDUwU")
  
  
  q<-paste(baseurl,"location=",coor,"&radius=",radius,"&types=food|restaurant&keyword=",keyword,"&key=",google_key, sep="")
  
  print(q)
  
  data1<-fromJSON(q)
  
  lat_long<-data.frame(lat=data1$results$geometry$location$lat,long=data1$results$geometry$location$lng)
  
  #print(data1)
  
  sitios<-data1$results$name
  
  df<-cbind(sitios,lat_long)
  return(df)
}

encontrar(lugar = coordenadas,radius = 500,"pizzeria")

##### working with NASA PDS #####
setwd("C:/rworking/pds")
getwd()

readme <- read.table("pdsdd.full",)

##### _____ Working with rerddap #####

##### install.packages for working with rerddap ##### 
install.packages("devtools")
install.packages("digest")
install.packages("data.table")
install.packages("DBI")
install.packages("assertthat")
install.packages("Rcpp")
install.packages("rerddap")
install.packages("magrittr")

library(magrittr)
library(Rcpp)
library(assertthat)
library(DBI)
library(devtools)
library(data.table)
library(digest)
library(rerddap)

##### using erddap #####
library(rerddap)
# search for key word 
x <- ed_search(query='deep', url = "https://ecowatch.ncddc.noaa.gov/erddap/")
x$alldata[[1]]
x$info

# list all datasets on server
x <- head(ed_datasets('table', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

x <- head(ed_datasets('grid', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

# Get info on a datasetid, then get data given information learned
info('deep_sea_corals', url = "https://ecowatch.ncddc.noaa.gov/erddap/")$variables

x <- tabledap('deep_sea_corals', 
         fields=c('latitude','longitude','ScientificName', "ImageURL"),
         url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]
        
View(x)

##### Exporting to KML ##### 

## Load required packages
#install.packages("maptools")
library(maptools)
library(rgdal)

##Set your working directory

setwd("C:/rworking/spatial_killa/inputs")

# load the live deep sea coral and sponge database occurrence locations via ERDDAP.

x <- tabledap('deep_sea_corals', 
              fields=c('latitude','longitude','ScientificName', "CatalogNumber", "FishCouncilRegion", "Locality",
                       "DataProvider", "Vessel", "VehicleName", "ObservationYear", "DepthInMeters", "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]
x <- data.frame(x)
x$latitude <- as.numeric(x$latitude)
x$longitude <- as.numeric(x$longitude)

# do some geographic subsetting

x <- filter(x, latitude > 25 , latitude < 30, longitude > -100)
#fix(x)

x <- filter(x, x$FishCouncilRegion == "North Pacific")

# # plot the xY coordinates
# 
# plot(x$longitude, x$latitude)

## create a SpatialPointsDataframe object and add the appropriate CRS

coordinates(x)<- c("longitude", "latitude")
proj4string(x)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML. 
# dsn should equal the name of the exported file and the dataset_options 
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/spatial_killa/outputs")
writeOGR(x, dsn="x.kml", layer= "Subset of NOAA's Deep Sea Coral and Sponge Database", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

## if you have Google Earth installed double click on the kml file you just created to open it. 
#The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity. 

##### _____ Using stringdist and clustering for large factors #####
# idea from: https://github.com/amunategui/string-distance-on-large-factors/blob/master/string-distance-on-large-factors.R

##### getting deep sea coral data using rerrdap::tabledap function #####
library(rerddap)
x <- tabledap('deep_sea_corals', 
              fields=c('latitude','longitude','ScientificName', "CatalogNumber", "FishCouncilRegion", "Locality",
                       "DataProvider", "Vessel", "VehicleName", "ObservationYear", 
                       "DepthInMeters", "ImageURL","SurveyID", "SamplingEquipment"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

# # just looking at the data that has images #
x1 <- x[is.na(x$ImageURL) == F,]

##### using the stringdist function to analyze short text clusters #####
# call the stringdistmatrix function and request some number of groups
#install.packages("stringdist")
library(stringdist)

# create a combined string using paste function 
x1$combined <- paste0(as.character(x1$Vessel),
                     "::", as.character(x1$VehicleName)#,
                     # " ", as.character(x$ObservationYear)#,
                     # " ", as.character(x$DataProvider)
)

#check the number of unique
print(length(unique(x1$combined)))

# create a vector of unique combined strings
unique <- unique(as.character(x1$combined))

# creating the distance matrix 
distancemodels <- stringdistmatrix(unique,unique,method = "jw")

# get names
rownames(distancemodels) <- unique

# make clusters
hc <- hclust(as.dist(distancemodels))

# visualize the dendrogram
plot(hc)
rect.hclust(hc,k=5)

# get tabular data and view
dfClust <- data.frame(unique, cutree(hc, k=5))
names(dfClust) <- c('modelname','cluster')
View(dfClust)

# visualize the groupings
plot(table(dfClust$cluster))
print(paste('Average number of models per cluster:', mean(table(dfClust$cluster))))

# look at the top clusters
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head (dfClust[c('cluster','modelname')],50)
View(dfClust)

#####  #####


