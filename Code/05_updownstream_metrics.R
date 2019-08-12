### Nov 8, 2018 
## Make metrics for area of upstream and downstream lakes 

#install libraries 
#install.packages("hydrolinks")
library(hydrolinks)
library(sf)
#install.packages("mapview")
library(mapview)
library(LAGOSNE)
#set a path for data 
cache_set_dir(path = 'Data/')

#load data 
only.MI.fish<-read.csv("~/Chap 2/Data/MI_lake_data.csv")

#get shapefiles from nhd Highres by using the permanent identifiers from LAGOS 
lake_poly = get_shape_by_id(only.MI.fish$permanent_, dataset = 'nhdh', feature_type = 'waterbody')
plot(st_geometry(lake_poly), col="Dodgerblue")

##test with just one lake first ### Duck lake COMID is 11950039 for med res
#change to an sf object   
sf_all_lakes<-sf::st_as_sf(lake_poly)
sf_duck<-dplyr::filter(sf_all_lakes, gnis_name == "Duck Lake" )  
#convert it back to a spatial polygons data frame
duck.sp<-as(sf_duck, "Spatial")

# traverse upstream
upstream = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'in')
upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
#return upstream stream length 
sum(upstream_streams$lengthkm)
#return lake area (sqkm) 
sum(upstream_lakes$areasqkm) 

#traverse downstream # the terminal reach for Duck Lake is 123335642
network = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'out')
### CROP OUT GREAT LAKES 
connected_streams = get_shape_by_id(network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
connected_lakes = get_shape_by_id(network$permanent_, dataset='nhdh', feature_type = 'waterbody')
GL_streams<-dplyr::filter(connected_streams, fcode == 56600 ) #coastline fcode
GL_lakes<-dplyr::filter(connected_lakes, fcode==39004) #Lake Superior fcode
new_network<-dplyr::anti_join(network, GL_streams, by="permanent_") %>%
                        dplyr::anti_join(GL_lakes, by="permanent_")
mapview(connected_streams)                  
#traverse back upstream from the terminal reach. This will get all of the water bodies  
return_network = traverse_flowlines(Inf, new_network[nrow(new_network),1], direction='in')
connected_streams = get_shape_by_id(return_network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')
#return downstream stream length 
mapview(connected_lakes) + mapview(connected_streams)

sum(connected_streams$lengthkm) - sum(upstream_streams$lengthkm)
#return downstream lake area (sqkm) *need to subtract GLs somehow 
sum(connected_lakes$areasqkm) - sum(upstream_lakes$areasqkm) - sf_duck$areasqkm

upstreams_sp<-sf::as_Spatial(upstream_streams)
streams_sp<-sf::as_Spatial(connected_streams)
lakes_sp<-sf::as_Spatial(connected_lakes)
mapview(connected_streams) + mapview(connected_lakes) 



## loop 
output <- data.frame(permanent_=NA, up_lakes_area=NA, up_streams_length=NA, down_lakes_area=NA, down_streams_length=NA)    

for (i in 1:length(lake_poly)) {
  perm<-lake_poly[i,1]
  
  # traverse upstream
  upstream = traverse_flowlines(Inf, perm, direction = 'in')
  upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  up_lakes_sum <- if(is.na(upstream_lakes)) {0}   
  else{ sum(upstream_lakes$areasqkm) #return upstream lake area (sqkm) 
  }
  up_streams_sum <-if(is.na(upstream_streams)) {0}  
  else{ sum(upstream_streams$lengthkm) #return upstream lake area (sqkm) 
  }
  
  #traverse downstream 
  network = traverse_flowlines(Inf, perm, direction = 'out')
  ### CROP OUT GREAT LAKES 
  connected_streams = get_shape_by_id(network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  connected_lakes = get_shape_by_id(network$permanent_, dataset='nhdh', feature_type = 'waterbody')
  GL_streams<-dplyr::filter(connected_streams, fcode == 56600 ) #coastline fcode
  GL_lakes<-dplyr::filter(connected_lakes, fcode==39004 | fcode==39010) #Lake Superior fcodes
  new_network<-dplyr::anti_join(network, GL_streams, by="permanent_") %>%
    dplyr::anti_join(GL_lakes, by="permanent_")
  network2<-dplyr::filter(new_network, !grepl("{", permanent_, fixed = TRUE)) #get rid of other polylines in GL
  network2$LENGTHnum<-as.numeric(network2$LENGTHKM) #need numeric to find max distance from focal lake 
  
  #traverse back upstream from the terminal reach. This will get all of the water bodies  
  return_network = traverse_flowlines(Inf, network2[which.max(network2$LENGTHnum),1], direction='in',  max_steps = 100000)
  connected_streams = get_shape_by_id(return_network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')
  
  down_lakes_sum <- sum(connected_lakes$areasqkm) - up_lakes_sum - lake_poly[i,6] #return downstream lake area (sqkm)= all lakes - upstream lakes - the focal lake area
  
  down_streams_sum <- sum(connected_streams$lengthkm) - up_streams_sum #return downstream stream length
  
  
  output[i,1]=perm
  output[i,2]=up_lakes_sum
  output[i,3]=up_streams_sum
  output[i,4]=down_lakes_sum
  output[i,5]=down_streams_sum
} 

######################## other tests ########################
#PLOTTING 
#traverse upstream of the lake #upstream traversal; plot all inflows
#specify a max traversal distance - set to infinity (Inf)

upstream = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'in')
#then use these to pull the line features and waterbodies (no upstream lakes returned)
upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')

###downstream
downstream = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'out')
downstream_stream = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
downstream_lakes = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'waterbody')

#turn the objects to spatial objects 
down_lakes_sp<-sf::as_Spatial(downstream_lakes)
down_streams_sp<-sf::as_Spatial(downstream_stream)
up_streams_sp<-sf::as_Spatial(upstream_streams)
## map 'SpatialPolygonsDataFrame'
mapview(duck.sp) + mapview(up_streams_sp) + mapview(down_streams_sp) + mapview(down_lakes_sp) 

## use the terminal stream to traverse upstream from Lake Superior 
## adjust max steps to get it to include everything (10,000 is the default which cut off some connections)
upfromSup<-traverse_flowlines(Inf, 123335642, direction="in", dataset=c("nhdh"), max_steps = 100000)
upfromSup_stream = get_shape_by_id(upfromSup$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upfromSup_lakes = get_shape_by_id(upfromSup$permanent_, dataset = 'nhdh', feature_type = 'waterbody')

alllakes_sp<-sf::as_Spatial(upfromSup_lakes)
allstreams_sp<-sf::as_Spatial(upfromSup_stream)
mapview(alllakes_sp) + mapview(allstreams_sp) 


#test another lake - Ross Lake 
Rosslake<-get_shape_by_id(149759308, dataset = 'nhdh', feature_type = 'waterbody')
#traverse upstream of the lake
upstream = traverse_flowlines(Inf, 149759308, direction = 'in')
#then use these to pull the line features and waterbodies (no upstream lakes returned)
upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
plot(st_geometry(upstream_streams), col='darkgreen')
plot(st_geometry(upstream_lakes), col='darkblue', add=TRUE)

###downstream
downstream = traverse_flowlines(Inf, 149759308, direction = 'out')
downstream_stream = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
downstream_lakes = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'waterbody')
plot(st_geometry(downstream_stream), col='palegreen')
plot(st_geometry(downstream_lakes), col='Dodgerblue', add=TRUE)
plot(st_geometry(Rosslake), col="Dodgerblue", add=TRUE)

#plot with mapview #turn the objects to spatial objects 
down_lakes_sp<-sf::as_Spatial(downstream_lakes)
down_streams_sp<-sf::as_Spatial(downstream_stream)
up_streams_sp<-sf::as_Spatial(upstream_streams)
up_lakes_sp<-sf::as_Spatial(upstream_lakes)
## map 'SpatialPolygonsDataFrame'
mapview(Rosslake) + mapview(up_lakes_sp)+ mapview(up_streams_sp) + mapview(down_streams_sp) + mapview(down_lakes_sp) + mapview(t_reach$geometry, color = "red")



################################################
# try out Joe's package to find the most downstream stream, then use Luke's package to walk upstream
devtools::install_github("jsta/nhdR")
library(nhdR)
#This package also requires an installation of 7-zip that can be called via the command line by typing 7z into the terminal 

# get a vpu export, VPU #4 is Michigan 
#you don't necessarily have to download everything before hand; "nhd_plus_load" would work an download if it doesnt exist. 
nhdR:::nhd_path()
nhd_plus_get(vpu = 4, "NHDSnapshot", force_unzip = TRUE)
nhd_plus_get(vpu = 4, "NHDPlusAttributes", force_unzip = TRUE)
nhd_plus_get(vpu = 4, "NHDPlusCatchment", force_unzip = TRUE)

nhd_plus_list(vpu = 4, "NHDSnapshot")
nhd_plus_list(vpu = 4, "NHDPlusAttributes")
nhd_plus_list(vpu = 4, "NHDPlusCatchment")

#network query lat long is the focal lake, leave everything else as 
#### Duck  Lake coords 46.20520 , -89.21884, however this code does not find the outlet
coords  <- data.frame(lat = 46.20520, lon = -89.21884)
#qry <- nhd_plus_query(coords$lon, coords$lat,
                  #    dsn = c("NHDWaterbody"), buffer_dist = 0.05)


t_reach <- terminal_reaches(coords$lon, coords$lat, buffer_dist = 0.01)

mapview(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)) +
  mapview(t_reach$geometry, color = "red")

#test that works with lake from Joe's example code 
coords  <- data.frame(lat = 41.42217, lon = -73.24189)
t_reach <- terminal_reaches(coords$lon, coords$lat)

mapview(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)) +
  mapview(t_reach$geometry, color = "red")

# test one lake; start with something not at great lakes !
#view all lakes 
mapview(st_as_sf(only.MI.fish, coords = c("nhd_long", "nhd_lat"), crs = 4326)) 

coords  <- data.frame(lat = 43.88355, lon = -84.49842)
t_reach <- terminal_reaches(coords$lon, coords$lat)
mapview(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)) +
  mapview(t_reach$geometry, color = "red")

### RIVER DISTANCE PACKAGE MAY BE USEFUL 
#install.packages("riverdist")
#library(riverdist)

##### Joe's code from streamnet package for closest lake metrics (could not download package)###
## function to use with igraph, findd this in Joe's code - sf2igraph 
devtools::install_github("jsta/streamnet")
closest_lake_distance()
closest_lake_distance <- function(lines, lakes, outlet, size_threshold = 4,
                                  map = FALSE){
  
  res <- get_focal_distance(lines, lakes, outlet, size_threshold)
  
  if(!all(is.na(res))){
    if(map){
      plot(st_sf(data.frame(dist = res$dist),
                 st_sfc(st_geometry(res$t_reach_pnts))))
    }
    
    list(
      closest_lake_distance = min(res$dist),
      num_up_lakes          = length(res$t_reach_pnts),
      lake_area             = res$lake_area)
  }else{
    list(
      closest_lake_distance = NA,
      num_up_lakes          = NA,
      lake_area             = NA)
  }
}



wb_coords <- c(44.00467, -88.43445)
res <- hlk_traverse(wb_coords)
hlk_traverse <- function(wb_coords, dset = "nhdh"){
  wb_id <- link_to_waterbodies(wb_coords[1], wb_coords[2],
                               1, dataset = 'nhdh')
  wb_id <- purrr::when(wb_id, any(names(.) %in% "COMID")
                       ~ .$COMID,
                       ~.$PERMANENT_)
  
  nhd_wb <- get_shape_by_id(wb_id, feature_type = "waterbody", dataset = dset)
  
  f_lines <- traverse_flowlines(max_distance = Inf, direction = "in",
                                start = wb_id,
                                dataset = dset, md5check = FALSE)
  f_lines <- purrr::when(f_lines, any(names(.) %in% "COMID")
                         ~ .$COMID,
                         ~.$PERMANENT_)
  
  upstream_shp <- get_shape_by_id(f_lines, dataset = dset,
                                  feature_type = "flowline")
  
  list(nhd_wb = nhd_wb, upstream_shp = upstream_shp)
}

#################################################
### NHDplusTools vs Joe's package nhdR
library(mapview)
library(nhdR)
install.packages("lwgeom")
library(lwgeom)
devtools::install_github("dblodgett-usgs/nhdplusTools")
library(nhdplusTools)
library(sf)
library(ggplot2)
library(ggspatial)

### example code for NHDplusTools that works 
comid            <- 12228511
nldi_comid <- list(featureSource = "comid",
                   featureID = comid)

dd <- navigate_nldi(nldi_feature = nldi_comid,
                    mode = "downstreamMain",
                    data_source = "")
mapview(dd)
ggplot() +
  geom_sf(data = st_geometry(dd)) 

#### USE NHDPLUS V2 medium res
#downstream and upstream lake areas #Note: Match_ID is perm ID from highres, lost some lakes 
linked_nhdplusv2 = link_to_waterbodies(only.MI.fish$nhd_lat, only.MI.fish$nhd_long,  only.MI.fish$permanent_, dataset = 'nhdplusv2', buffer = 30)

#try with Duck Lake ## the comID for the outflowing stream is 11951679
comid            <- 11951679

nldi_comid <- list(featureSource = "comid",
                   featureID = comid)

dd <- nhdplusTools::navigate_nldi(nldi_feature = nldi_comid,
                    mode = "downstreamMain",
                    data_source = "comid")


#nav_options <- discover_nldi_navigation(nldi_comid) ## look into what this does  need a sources id and a feature ID 

ggplot() +
   geom_sf(data = st_geometry(dd)) +
  geom_sf(data = point, color = "darkgreen", add=TRUE)

mapview(dd)

########################### Trying to re-do Luke's CODE ! ##############
### neighbors function needed for traverse function 
neighbors = function(node, direction = c("in", "out"), dataset = c("nhdh", "nhdplusv2"), con){
  From_Permanent_Identifier = NULL
  To_Permanent_Identifier = NULL
  
  direction = match.arg(direction)
  dataset = match.arg(dataset)
  dinfo = dataset_info(dataset, "flowline")
  from_column = dinfo$flowtable_from_column
  to_column = dinfo$flowtable_to_column
  
  sql = ""
  
  if(direction == "out"){
    sql = paste0("SELECT * from flowtable where ", from_column, " IN ('", paste(node, collapse = "','"), "')")
  }
  else if(direction == "in"){
    sql = paste0("SELECT * from flowtable where ", to_column, " IN ('", paste(node, collapse = "','"), "')")
  }
  graph = DBI::dbGetQuery(con, sql)
  ret = NULL
  if(direction == "out"){
    ret = data.frame(graph[, to_column], graph$LENGTHKM)
  }
  else if(direction == "in"){
    ret = data.frame(graph[, from_column], graph$LENGTHKM)
  }
  colnames(ret) = c("ID", "LENGTHKM")
  ret$ID = as.character(ret$ID)
  return(ret)
}
#traverse_flowlines code 
k_traverse<-function (max_distance, start, direction = c("out", "in"), dataset = c("nhdh", 
                                                                       "nhdplusv2"), max_steps = 10000, db_path = NULL, ...) 
{
  direction = match.arg(direction)
  dataset = match.arg(dataset)
  dinfo = dataset_info(dataset, "flowline")
  check_dl_file(system.file("extdata/shape_id_cache.csv", package = "hydrolinks"), 
                fname = paste0(dataset, "_flowline_ids.zip"))
  if (is.null(db_path)) {
    db_name = paste0("flowtable_", dataset)
    check_dl_file(system.file("extdata/flowtable.csv", package = "hydrolinks"), 
                  fname = paste0(db_name, ".zip"), ...)
    con = DBI::dbConnect(RSQLite::SQLite(), file.path(cache_get_dir(), 
                                                 "unzip", paste0(db_name, ".zip"), paste0(db_name, 
                                                                                          ".sqlite3")))
  }
  else {
    con = dbConnect(RSQLite::SQLite(), db_path)
  }
  if (is.null(start) || start == 0) {
    dbDisconnect(con)
    stop("Cannot traverse from node 0!")
  }
  nodes = data.frame(rep(NA, max_steps), rep(NA, max_steps), 
                     rep(NA, max_steps), stringsAsFactors = FALSE)
  colnames(nodes) = c(dinfo$id_column, "LENGTHKM", "CHILDREN")
  iterations = 1
  n = neighbors(start, direction, dataset, con)
  if (nrow(n) == 0) {
    flowline = get_shape_by_id(start, feature_type = "flowline", 
                               dataset = dataset, match_column = dinfo$id_column)
    if (!is.na(flowline) && !is.na(flowline[, dinfo$virtual_fl_waterbody_column])) {
      st_geometry(flowline) = NULL
      warning(paste0("Start ID provided is a virtual flowline inside a waterbody. Continuing from ", 
                     flowline[1, dinfo$virtual_fl_waterbody_column]))
      n = neighbors(flowline[, dinfo$virtual_fl_waterbody_column], 
                    direction, dataset, con)
    }
    else {
      nodes = nodes[1, ]
      dbDisconnect(con)
      return(nodes)
    }
  }
  n$LENGTHKM[is.na(n$LENGTHKM)] = 0
  to_check = n$LENGTHKM
  names(to_check) = n$ID
  if (max_distance == 0) {
    nodes = cbind(names(to_check), to_check, NA, NA)
    rownames(nodes) = c(1:nrow(nodes))
    dbDisconnect(con)
    return(nodes)
  }
  while (1) {
    next_check = c()
    to_check = to_check[names(to_check) != "0"]
    to_check = to_check[which(!(names(to_check) %in% nodes[, 
                                                           1]))]
    if (length(to_check) == 0) {
      nodes = nodes[!is.na(nodes[, dinfo$id_column]), ]
      dbDisconnect(con)
      return(nodes)
    }
    if (length(names(to_check)) > 1) {
      nodes[c(iterations:(iterations + length(names(to_check)) - 
                            1)), ] = cbind(names(to_check), to_check, NA)
    }
    else {
      nodes[iterations, ] = cbind(names(to_check), to_check, 
                                  NA)
    }
    iterations = iterations + length(to_check)
    for (j in 1:length(to_check)) {
      n = neighbors(names(to_check)[j], direction, dataset, 
                    con)
      nodes[which(nodes[, 1] == names(to_check)[j]), 3] = paste(n$ID, 
                                                                sep = ",", collapse = ",")
      n$LENGTHKM[is.na(n$LENGTHKM)] = 0
      next_check_tmp = n$LENGTHKM + to_check[j]
      names(next_check_tmp) = n$ID
      next_check = c(next_check, next_check_tmp)
    }
    next_check = next_check[unique(names(next_check))]
    if (iterations > max_steps) {
      next_nodes = data.frame(names(next_check), next_check, 
                              "STUCK")
      colnames(next_nodes) = c(dinfo$id_column, "LENGTHKM", 
                               "CHILDREN")
      nodes = rbind(nodes, next_nodes)
      nodes = nodes[!is.na(nodes[, dinfo$id_column]), ]
      dbDisconnect(con)
      return(nodes)
    }
    if (max_distance < 0 && any(names(next_check) == "0")) {
      nodes = nodes[!is.na(nodes[, dinfo$id_column]), ]
      dbDisconnect(con)
      return(nodes)
    }
    if (all(names(next_check) == "0")) {
      nodes = nodes[!is.na(nodes[, dinfo$id_column]), ]
      dbDisconnect(con)
      return(nodes)
    }
    for (j in names(next_check)) {
      if (max_distance > 0 && next_check[j] > max_distance) {
        nodes = nodes[!is.na(nodes[, dinfo$id_column]), 
                      ]
        dbDisconnect(con)
        return(nodes)
      }
    }
    to_check = next_check
  }
}
