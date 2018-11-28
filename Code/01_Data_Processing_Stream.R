#### Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Oct 15, 2018


#load libraries
library(rgdal)
library(sp)
library(dplyr)
library(raster)


stream_fish<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/Fish streams/stream_fish_5_states.csv", header = TRUE)
streampoints<-dplyr::select(stream_fish, siteid_loc, lat, lon)
streampoints<- streampoints[!duplicated(paste(streampoints$siteid_loc)),] #select only one row of each stream
x <- streampoints$lon
y <- streampoints$lat

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
stream.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")
stream.aea <- spTransform(stream.ll, prjnew)
plot(stream.aea)

#use the over function to match points to LAGOS HUC4s- select MI HUC4s only 
#bring in HU4 polygons 
HU4.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU4", layer = "HU4")
names(HU4.poly)
crs(HU4.poly) #shows me the projection of the shapefiles so that I can project the same to the points 
##change to an sf object   
sf_HU4<-sf::st_as_sf(HU4.poly)
#have to have the zone id in there every time for this to work 
HU4MI<-dplyr::filter(sf_HU4, ZoneID == "HU4_24" | ZoneID == "HU4_31" | ZoneID =="HU4_30"|ZoneID == "HU4_37"| ZoneID =="HU4_24" |ZoneID =="HU4_40" |ZoneID =="HU4_39" |ZoneID =="HU4_32"|ZoneID == "HU4_41" )  
#convert it back to a spatial polygons data frame
HU4MI.sp<-as(HU4MI, "Spatial")
crs(HU4MI.sp)
plot(HU4MI.sp)
crs(stream.aea) <- crs(HU4MI.sp) # points to the same projection

#Returns a data.frame of the second argument with row entries corresponding to the first argument
MI.fish.streams<-sp::over(stream.aea, HU4MI.sp) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
MI.fish.streams$siteid_loc<-paste(streampoints$siteid_loc)  
streams.matchHU4<-MI.fish.streams[!is.na(MI.fish.streams$ZoneID),] 
streams.matchHU4<-dplyr::select(streams.matchHU4, siteid_loc, ZoneID)

##add HU4 IDs to the streams data 
stream.merge.HU4<-dplyr::left_join(stream_fish, streams.matchHU4)

#filter out only MI streams 
MI_stream_data<-stream.merge.HU4[!is.na(stream.merge.HU4$ZoneID),] 
write.csv(MI_stream_data, "Data/MI_stream_data.csv", row.names = FALSE)

x <- MI_stream_data$lon
y <- MI_stream_data$lat

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
stream.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")
stream.aea <- spTransform(stream.ll, prjnew)
plot(stream.aea)