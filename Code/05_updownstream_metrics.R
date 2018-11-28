### Nov 8, 2018 
## Make metrics for area of upstream and downstream lakes 

install.packages("hydrolinks")
library(hydrolinks)
library(sf)
#set a path for data 
cache_set_dir(path = 'Data/')

#get shapefiles from nhd Highres by using the permanent identifiers from LAGOS 
lake_poly = get_shape_by_id(only.MI.fish$Permanent_, dataset = 'nhdh', feature_type = 'waterbody')
plot(st_geometry(lake_poly), col="Dodgerblue")

#upstream traversal; plot all inflows, 
#start from the lake and move up the hydrologic network. 
#specify a max traversal distance - set to infinity (Inf)

##test with just one lake first 
#change to an sf object   
sf_all_lakes<-sf::st_as_sf(lake_poly)
sf_duck<-dplyr::filter(sf_all_lakes, gnis_name == "Duck Lake" )  
#convert it back to a spatial polygons data frame
duck.sp<-as(sf_duck, "Spatial")
plot(duck.sp)
#try this to see if it will work with the st_geometry function (NOPE!) 
duck.sf<-sf::as_Spatial(sf_duck)

#traverse upstream of the lake
upstream = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'in')
#then use these to pull the line features and waterbodies (no upstream waterbodies returned)
upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
plot(duck.sp)
plot(upstream_streams, col='darkgreen', add=TRUE)

###downstream
downstream = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'out')
downstream_stream = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
downstream_lakes = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'waterbody')
plot(st_geometry(downstream_stream), col='palegreen')
plot(st_geometry(downstream_lakes), col='Dodgerblue', add=TRUE)
plot(duck.sp, add=TRUE)

#turn the objects to spatial objects 
down_lakes_sp<-sf::as_Spatial(downstream_lakes)
down_streams_sp<-sf::as_Spatial(downstream_stream)
up_streams_sp<-sf::as_Spatial(upstream_streams)

## use the stream to traverse upstream from Lake Superior 
upfromSup<-traverse_flowlines(Inf, 123335642, direction="in", dataset=c("nhdh"), max_steps = 100000)
upfromSup_stream = get_shape_by_id(upfromSup$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upfromSup_lakes = get_shape_by_id(upfromSup$permanent_, dataset = 'nhdh', feature_type = 'waterbody')

alllakes_sp<-sf::as_Spatial(upfromSup_lakes)
allstreams_sp<-sf::as_Spatial(upfromSup_stream)
mapview(alllakes_sp) + mapview(allstreams_sp) 

##mapview package 
install.packages("mapview")
library(mapview)

## S4 method for signature 'SpatialPolygonsDataFrame'
mapview(duck.sp) + mapview(up_streams_sp) + mapview(down_streams_sp) + mapview(down_lakes_sp) 

### some way to delete flow lines and just walk all lines and polys?! 
# try out Joe's package to find the most downstream stream, then use Luke's package to walk upstream
devtools::install_github("jsta/nhdR")
library(nhdR)
#also need to unzip 7-zip, so need for MAC 
#There is not yet any GUI app for any other OS, but there are two command-line tools, unar and lsar, 
#which can be used on macOS
#Ubuntu 12.04 packages unar and lsar, under the package name "unar". 
install.packages("unar")
#You can install them from the Software Center, or using the command line:
#sudo apt-get install unar

#install.packages("riverdist")
#library(riverdist)




library(sf)
library(mapview)

coords  <- data.frame(lat = 42.96628 , lon = -89.25264)
t_reach <- terminal_reaches(coords$lon, coords$lat)

coords  <- data.frame(lat = 20.79722, lon = -156.47833)
t_reach <- terminal_reaches(coords$lon, coords$lat)

coords  <- data.frame(lat = 41.42217, lon = -73.24189)
t_reach <- terminal_reaches(coords$lon, coords$lat)

