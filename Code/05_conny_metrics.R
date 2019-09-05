#load libraries 
#install.packages("hydrolinks")
library(dplyr)
library(hydrolinks)
library(sf)
library(mapview)
library(LAGOSNE)

#set a path for data from hydrolinks package 
cache_set_dir(path = 'Data/')

#read in data 
only.MI.fish<-read.csv("Data/MI_lake_data.csv")

####### Get downstream and upstream lake areas ###### 

#Note: Match_ID is perm ID from highres, lost some lakes if linking to nhdPlus
    #linked_nhdplusv2 = link_to_waterbodies(only.MI.fish$nhd_lat, only.MI.fish$nhd_long,  only.MI.fish$permanent_, dataset = 'nhdplusv2', buffer = 30)

#get shapefiles from nhd high-res by using the permanent identifiers from LAGOS #lost one lake  
lake_poly = get_shape_by_id(only.MI.fish$permanent_, dataset = 'nhdh', feature_type = 'waterbody')

## test one lake Duck Lake connected to Lake Superior 
test<-filter(lake_poly, permanent_ == 123397648)
#test with two lakes 
#test<-filter(lake_poly, permanent_ == 123397648 | permanent_ ==     123396104 )
#test<-as.data.frame(test)

output <- data.frame(permanent_=NA, up_lakes_area=NA, down_lakes_area=NA, closest_lake_area=NA, stream_distance_lake=NA)            

for (permanent_ in test) {
  # traverse upstream
  upstream = traverse_flowlines(Inf, test$permanent_, direction = 'in')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  
  if(!is.na(upstream_lakes)) {
    up_lakes_sum = sum(upstream_lakes$areasqkm) #return total upstream lake area (sqkm)
    conn_lakes<-as.data.frame(upstream_lakes)
    lakes<-dplyr::left_join(conn_lakes, upstream, by = "permanent_")
    lakes$LENGTHnum<-as.numeric(lakes$LENGTHKM)  #need numeric to find closest lake
    up_stream_dist_lake = lakes[which.min(lakes$LENGTHnum),19] # stream distance to the closest lake
    up_closest_lake_area=lakes[which.min(lakes$LENGTHnum),6]  #lake areasqkm of closest lake
    }   
  else{ 
      up_lakes_sum    = 0
      up_stream_dist_lake = 0
      up_closest_lake_area  = 0
  }
  
  #traverse downstream 
  downstream = traverse_flowlines(Inf, test$permanent_, direction = 'out')
  ### CROP OUT GREAT LAKES 
  downstream_streams = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  downstream_lakes = get_shape_by_id(downstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  GL_streams<-dplyr::filter(downstream_streams, fcode == 56600 ) #coastline fcode
  GL_lakes<-dplyr::filter(downstream_lakes, fcode==39004 | fcode==39010 | fcode==39000) #Lake Superior, Erie, Huron, Ontario  fcodes
  network<-dplyr::anti_join(downstream, GL_streams, by="permanent_") %>%
    dplyr::anti_join(GL_lakes, by="permanent_")
  network2<-dplyr::filter(network, !grepl("{", permanent_, fixed = TRUE)) #get rid of other polylines in GL
  network2$LENGTHnum<-as.numeric(network2$LENGTHKM) #need numeric to find distances from focal lake 
  
  #traverse back upstream from the terminal reach. This will get all of the water bodies  
  return_network = traverse_flowlines(Inf, network2[which.max(network2$LENGTHnum),1], direction='in',  max_steps = 100000)
  connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')
  
  if(!is.na(connected_lakes)) {
    conn_lakes<-as.data.frame(connected_lakes)
    lakes<-dplyr::left_join(conn_lakes, return_network, by = "permanent_")
    lakes$LENGTHnum<-as.numeric(lakes$LENGTHKM)  
    down_closest_lake_area=lakes[which.min(lakes$LENGTHnum),6]  #closest lake areasqkm
    down_stream_dist_lake=lakes[which.min(lakes$LENGTHnum),19]
    }
  else {
    down_closest_lake_area=0  
    down_stream_dist_lake=0
  }   
   
  
  stream_distance_lake<-  min(down_stream_dist_lake, up_stream_dist_lake)
  #fix this to be just the closest lake: 
    #closest_lake_area <-   max(down_closest_lake_area, up_closest_lake_area)
  
  down_lakes_sum <- sum(connected_lakes$areasqkm) - up_lakes_sum - test$areasqkm #return downstream lake area (sqkm)= all lakes - upstream lakes - the focal lake area
  
  output[,1]=test$permanent_
  output[,2]=up_lakes_sum
  output[,3]=down_lakes_sum
  output[,4]=closest_lake_area
  output[,5]=stream_distance_lake 
} 

mapview(connected_lakes)


## loop to traverse upstream metrics for all lakes 
## for all the lakes lake_poly[i]
lake_poly<-as.data.frame(lake_poly)

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




#### Connectivity metrics that are already in LAGOS ######
#install.packages("LAGOSNE")
library(LAGOSNE)
lg <- lagosne_load("1.087.1")

#watershed scale connectivity 
IWS_LULC <- lg$iws.lulc
IWS_LULC <- data.frame(lagoslakeid=IWS_LULC$lagoslakeid,
                       iws_wetland_woody2006_pct=IWS_LULC$iws_nlcd2006_pct_90,
                       iws_wetland_emergent2006_pct=IWS_LULC$iws_nlcd2006_pct_95)

# % wetland in the watershed
IWS_LULC$iws_nlcd2006_wet <- IWS_LULC$iws_wetland_woody2006_pct +
  IWS_LULC$iws_wetland_emergent2006_pct

# iws connectivity
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        conn_class=lake_conn$lakeconnection,
                        iws_upstream_lakes_4ha_area_ha=lake_conn$upstream_lakes_4ha_area_ha,
                        iws_wlconnections_area_ha=lake_conn$wlconnections_allwetlands_contributing_area_ha)

IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid,
                       iws_streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha)


#merge all local predictors
local_preds <- left_join(IWS_LULC, lake_conn, by = "lagoslakeid") %>%
  left_join(IWS_conn, by = "lagoslakeid")


## Merge LAGOS to MI lakes 
local_preds$lagoslakeid<-as.factor(local_preds$lagoslakeid)
fish.local<-left_join(fish.LAGOS, local_preds, by = 'lagoslakeid')

########################################
##### CODE FOR LAURA/ QIs tables ###### 
########################################
# Get all flow and Strahler info 
 # The input file geodatabase
library(rgdal)
dat <- "/Users/katelynking/Desktop/NHDPLUS_H_0101_HU4_GDB/NHDPLUS_H_0101_HU4_GDB.gdb"
dat2 <- "/Users/katelynking/Desktop/NHD_medres/NHD_H_0406_HU4_GDB.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(dat2)
print(fc_list)

#create a loop to read in all files for hi-res 
setwd("/Users/katelynking/Desktop/NHD_hires/")
ldf <- list() # creates a empty list
listgdb <- dir(pattern = "*.gdb") # creates the list of all the files in the directory
for (k in 1:length(listgdb)){
  EROM <- sf::st_read(listgdb[k], layer="NHDPlusEROMMA") #has flow info
  Flowline <- sf::st_read(listgdb[k], layer="NHDPlusFlowlineVAA") #has stream order
  crosswalk <- sf::st_read(listgdb[k], layer="NHDPlusFlow")
  flow <-dplyr::select(EROM, NHDPlusID, QEMA)
  strahler <-dplyr::select(Flowline, NHDPlusID, StreamOrde)
  IDs<-dplyr::select(crosswalk, FromNHDPID, FromPermID)
  colnames(IDs)[colnames(IDs)=="FromNHDPID"] <- "NHDPlusID"
  colnames(IDs)[colnames(IDs)=="FromPermID"] <- "PID"
  HUC0<-dplyr::left_join(flow, strahler) %>%
    left_join(IDs)
  write.csv(HUC0, file.path("/Users/katelynking/Desktop/NHD_stream/", paste(listgdb[k], ".csv", sep="")), row.names = FALSE)
 
}

#run loop for med res data. Downloaded Aug 5, 2019, just need reach code  
setwd("/Users/katelynking/Desktop/NHD_medres/")
  #colnames(IDs)[colnames(IDs)=="Permanent_Identifier"] <- "PID"

H401<-sf::st_read('NHD_H_0401_HU4_GDB.gdb', layer="NHDFlowline") %>%  #get reach code and permIDs to match High-res and med-res
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()
H402<-sf::st_read('NHD_H_0402_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H403<-sf::st_read('NHD_H_0403_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H404<-sf::st_read('NHD_H_0404_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H405<-sf::st_read('NHD_H_0405_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H406<-sf::st_read('NHD_H_0406_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H407<-sf::st_read('NHD_H_0407_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H408<-sf::st_read('NHD_H_0408_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H409<-sf::st_read('NHD_H_0409_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H410<-sf::st_read('NHD_H_0410_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H411<-sf::st_read('NHD_H_0411_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H412<-sf::st_read('NHD_H_0412_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H413<-sf::st_read('NHD_H_0413_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()
H414<-sf::st_read('NHD_H_0414_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode)  %>%
  st_drop_geometry()

#NOTE: trying to join all crashes my computer, will do each seperately  
### Use Joe's package for the rest of the NHDPlus data 
# install.packages("devtools")
devtools::install_github("jsta/nhdR")
library(nhdR)
## need VPUs (Vector Processing Units) 04, 08, 18, part of 13?? 
# get a vpu export 04
nhd_plus_get(vpu = 4, "NHDPlusAttributes")
nhd_plus_list(vpu = 4, "NHDPlusAttributes")
st_order04<-nhd_plus_load(vpu = 4, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow04 <- nhd_plus_load(4, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat04<-left_join(st_order04, eromflow04) #merge

#list<-c(H401, H402, H403, H404, H405, H406, H407, H408, H409, H410, H411, H412, H413, H414)
#list<-c('H401', 'H402', 'H403','H404', 'H405', 'H406', 'H407', 'H408', 'H409', 'H410', 'H411', 'H412', 'H413', 'H414')#Function wont work!! 
#for (i in 1:length(list)){
 # join<-dplyr::left_join(dat04, list[i])
  # colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
  # write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/", paste(list[i], ".csv", sep="")), row.names = FALSE)
#}

join<-dplyr::left_join(dat04, H401) #merge the med res data with the IDs from high res by reach code 
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H401.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H402)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H402.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H403)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H403.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H404)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H404.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H405)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H405.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H406)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H406.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H407)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H407.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H408)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H408.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H409)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H409.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H410)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H410.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H411)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H411.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H412)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H412.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H413)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H413.csv"), row.names = FALSE)

join<-dplyr::left_join(dat04, H414)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H414.csv"), row.names = FALSE)

#get 08 HUC
H801<-sf::st_read('NHD_H_0801_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H802<-sf::st_read('NHD_H_0802_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H803<-sf::st_read('NHD_H_0803_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H804<-sf::st_read('NHD_H_0804_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H805<-sf::st_read('NHD_H_0805_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H806<-sf::st_read('NHD_H_0806_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H807<-sf::st_read('NHD_H_0807_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H808<-sf::st_read('NHD_H_0808_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

H809<-sf::st_read('NHD_H_0809_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

nhd_plus_get(vpu = 8, "NHDPlusAttributes")
st_order08<-nhd_plus_load(vpu = 8, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow08 <- nhd_plus_load(8, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat08<-left_join(st_order08, eromflow08)#merge

join<-dplyr::left_join(H801, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H801.csv"), row.names = FALSE)

join<-dplyr::left_join(H802, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H802.csv"), row.names = FALSE)

join<-dplyr::left_join(H803, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H803.csv"), row.names = FALSE)

join<-dplyr::left_join(H804, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H804.csv"), row.names = FALSE)

join<-dplyr::left_join(H805, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H805.csv"), row.names = FALSE)

join<-dplyr::left_join(H806, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H806.csv"), row.names = FALSE)

join<-dplyr::left_join(H807, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H807.csv"), row.names = FALSE)

join<-dplyr::left_join(H808, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H808.csv"), row.names = FALSE)

join<-dplyr::left_join(H809, dat08)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H809.csv"), row.names = FALSE)

#get 18 series 1802, 1803, 1804,1805
nhd_plus_get(vpu = 18, "NHDPlusAttributes")
st_order18<-nhd_plus_load(vpu = 18, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow18 <- nhd_plus_load(18, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat18<-left_join(st_order18, eromflow18)#merge

H1802<-sf::st_read('NHD_H_1802_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()
H1803<-sf::st_read('NHD_H_1803_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()
H1804<-sf::st_read('NHD_H_1804_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()
H1805<-sf::st_read('NHD_H_1805_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

join<-dplyr::left_join(H1802, dat18)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1802.csv"), row.names = FALSE)

join<-dplyr::left_join(H1803, dat18)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1803.csv"), row.names = FALSE)

join<-dplyr::left_join(H1804, dat18)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1804.csv"), row.names = FALSE)

join<-dplyr::left_join(H1805, dat18)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1805.csv"), row.names = FALSE)

## OTHERS 1602, 1702, 0313, 0701
  #03
H0313<-sf::st_read('NHD_H_0313_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

st_order03<-nhd_plus_load(vpu='03W', component='NHDPlusAttributes',
              dsn='PlusFlowlineVAA', approve_all_dl=TRUE) %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow03 <- nhd_plus_load(vpu='03W', "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat03<-left_join(st_order03, eromflow03)#merge

join<-dplyr::left_join(H0313, dat03)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H0313.csv"), row.names = FALSE)

 #07
H0701<-sf::st_read('NHD_H_0701_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

nhd_plus_get(vpu = 7, "NHDPlusAttributes")
st_order07<-nhd_plus_load(vpu = 7, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow07 <- nhd_plus_load(7, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat07<-left_join(st_order07, eromflow07)#merge

join<-dplyr::left_join(H0701, dat07)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H0701.csv"), row.names = FALSE)
  
  #16
H1602<-sf::st_read('NHD_H_1602_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

nhd_plus_get(vpu = 16, "NHDPlusAttributes")
st_order16<-nhd_plus_load(vpu = 16, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow16 <- nhd_plus_load(16, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat16<-left_join(st_order16, eromflow16)#merge

join<-dplyr::left_join(H1602, dat16)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1602.csv"), row.names = FALSE)

#17
H1702<-sf::st_read('NHD_H_1702_HU4_GDB.gdb', layer="NHDFlowline") %>%
  dplyr::select(Permanent_Identifier, ReachCode) %>%
  st_drop_geometry()

nhd_plus_get(vpu = 17, "NHDPlusAttributes")
st_order17<-nhd_plus_load(vpu = 17, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde, ReachCode) #has stream order
eromflow17 <- nhd_plus_load(17, "EROMExtension", "EROM_MA0001") %>%  # has stream flow
  dplyr::select(ComID, Q0001F)
dat17<-left_join(st_order17, eromflow17)#merge

join<-dplyr::left_join(H1702, dat17)
colnames(join)[colnames(join)=="Permanent_Identifier"] <- "PID"
write.csv(join, file.path("/Users/katelynking/Desktop/NHDmed_streams/H1702.csv"), row.names = FALSE)


##########################
### TRY TO MAKE MAPS OF SOME CHAINS   ######
locus<-dplyr::select(lg$locus, lagoslakeid, nhdid, nhd_long, nhd_lat)
lake8047<-filter(locus, lagoslakeid == "8047")
lake7922<-filter(locus, lagoslakeid == "7922")
unknown_lake<-filter(locus, nhdid=="152430225")
#get shapefiles from nhd high-res by using the permanent identifiers from LAGOS 
lake_poly = get_shape_by_id(lake8047$nhdid, dataset = 'nhdh', feature_type = 'waterbody')
lake_poly2 = get_shape_by_id(unknown_lake$nhdid, dataset = 'nhdh', feature_type = 'waterbody')
upstream = traverse_flowlines(Inf,lake_poly$permanent_, direction = 'in')
upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
#traverse downstream 
downstream = traverse_flowlines(Inf, lake_poly$permanent_, direction = 'out')
downstream_lakes = get_shape_by_id(downstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
downstream_streams = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')

#upstream of moosehead lake 
upstream_moose = traverse_flowlines(Inf,lake_poly2$permanent_, direction = 'in')
upstream_moose_lakes = get_shape_by_id(upstream_moose$permanent_, dataset='nhdh', feature_type = 'waterbody')
upstream_moose_streams = get_shape_by_id(upstream_moose$permanent_, dataset = 'nhdh', feature_type = 'flowline')

### would need to filter out all lakes <1 hectare or <0.015 sqkm 
#837 upstream lakes
up_lakes<-filter(upstream_moose_lakes, areasqkm >= 0.010)
#traverse back upstream from the terminal reach. This will get all of the water bodies  (note this is too much- goes across HUCS)
return_network = traverse_flowlines(Inf, downstream[which.max(downstream$LENGTHKM),1], direction='in',  max_steps = 100000)
connected_streams = get_shape_by_id(return_network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')

#mapview(connected_streams) + mapview(connected_lakes) 
mapview(upstream_lakes, color="blue") + mapview(upstream_streams, color="green") +
   mapview(downstream_streams, color="green") +
  mapview(upstream_moose_lakes, color="blue") + mapview(upstream_moose_streams, color="green")

#do Lauras LAGOSids match mine? 
#lagoslake 8206 has 3 PIDs that it matches, 152428883, 152428884, 152428885 
locus<-dplyr::select(lg$locus, lagoslakeid, nhdid, nhd_long, nhd_lat)
lake8206<-filter(locus, lagoslakeid == "8206")
coords <- coordinatize(lake8206)
mapview(coords) 
lake_83<-filter(locus, nhdid=="152428883") # not in NE
lake_84<-filter(locus, nhdid=="152428884")  # not in NE
lake_85<-filter(locus, nhdid=="152428885")  # not in NE
lake_7467<-filter(locus, lagoslakeid == "7467")
coords7467 <- coordinatize(lake_7467)
mapview(coords7467)
#in LAGOS NE lake 8206 has an nhd ID of 152433202 - pull it from the hydrolinks 
tarsey = get_shape_by_id(152433202, dataset='nhdh', feature_type = 'waterbody')
mapview(tarsey) 
m_lake = get_shape_by_id(152433316, dataset='nhdh', feature_type = 'waterbody')
mapview(m_lake) 

#this doesnt exist was a "waterbody" it is a stream connector through the lake
lake_85=get_shape_by_id(152428885, dataset='nhdh', feature_type = 'flowline')
mapview(lake_83)
stream_1=get_shape_by_id(152413256, dataset='nhdh', feature_type = 'flowline')
stream_2=get_shape_by_id(152413257, dataset='nhdh', feature_type = 'flowline')
stream_3=get_shape_by_id(152429037, dataset='nhdh', feature_type = 'flowline')
stream_4=get_shape_by_id(152429036, dataset='nhdh', feature_type = 'flowline')
stream_5=get_shape_by_id(152413254, dataset='nhdh', feature_type = 'flowline')
stream_6=get_shape_by_id(152413255, dataset='nhdh', feature_type = 'flowline')
stream_7=get_shape_by_id(152429045, dataset='nhdh', feature_type = 'flowline')

mapview(lake_85) + mapview(stream_1) + mapview(stream_2) + mapview(stream_3) + mapview(stream_4) +
  mapview(stream_5) +mapview(stream_6) + mapview(stream_7) + mapview(m_lake)

### Inlet to Torsey Lake MAINE 
lake56<-filter(locus, lagoslakeid == "122356")
lake91 = get_shape_by_id(152433191, dataset='nhdh', feature_type = 'waterbody')
stream_83=get_shape_by_id(152428883, dataset='nhdh', feature_type = 'flowline')
stream_84=get_shape_by_id(152428884, dataset='nhdh', feature_type = 'flowline')
stream_87=get_shape_by_id(152428887, dataset='nhdh', feature_type = 'flowline')
stream_59=get_shape_by_id(152413259, dataset='nhdh', feature_type = 'flowline')

mapview(tarsey) + mapview(stream_83, color="green") + mapview(stream_84, color="blue") + mapview(stream_87, color="blue") + 
  mapview(stream_59, color="blue") + mapview(lake91) + mapview(lake_85, color='red') + 
  mapview(stream_1, color='red') + mapview(stream_2, color='red') + mapview(stream_3, color='red') + mapview(stream_4, color='red') +
  mapview(stream_5, color='red') +mapview(stream_6, color='red') + mapview(stream_7, color='red') + mapview(m_lake, color='red')

##########################
### Check Lake Orders ######
locus<-dplyr::select(lg$locus, lagoslakeid, nhdid, nhd_long, nhd_lat)
lake10134<-filter(locus, lagoslakeid == "10134")
coords <- coordinatize(lake10134)
mapview(coords) 
lake_head = get_shape_by_id(152432943, dataset='nhdh', feature_type = 'waterbody')
lake70495<-filter(locus, lagoslakeid == "70495")
lake_2=get_shape_by_id(152432752, dataset='nhdh', feature_type = 'waterbody')
lake19430<-filter(locus, lagoslakeid == "19430")
lake_3=get_shape_by_id(152432762, dataset='nhdh', feature_type = 'waterbody')

lake_4=get_shape_by_id(152433096, dataset='nhdh', feature_type = 'waterbody')
lake40455 <-filter(locus, lagoslakeid == "40455")
lake_5=get_shape_by_id(152432974, dataset='nhdh', feature_type = 'waterbody')

#traverse downstream 
downstream = traverse_flowlines(Inf, lake10134$nhdid, direction = 'out')
### CROP OUT GREAT LAKES 
downstream_streams = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
downstream_lakes = get_shape_by_id(downstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
mapview(lake_head, color="orange") + mapview(downstream_lakes) + mapview(downstream_streams, color="green") + 
  mapview(up_streams, color="red") + mapview(lake_2, color="yellow")

#traverse downstream 
upstream = traverse_flowlines(Inf, lake10134$nhdid, direction = 'in')
up_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')

#upstream of lake 2
up2 = traverse_flowlines(Inf, lake70495$nhdid, direction = 'in')
up_streams2 = get_shape_by_id(up2$permanent_, dataset = 'nhdh', feature_type = 'flowline')
mapview(lake_2, color="yellow") + mapview(up_streams2, color="yellow") + mapview(lake_head, color="orange") + 
  mapview(lake_3, color="green") + mapview(lake_4, color="purple")

up4 = traverse_flowlines(Inf, lake8339$nhdid, direction = 'in')
up_streams4 = get_shape_by_id(up4$permanent_, dataset = 'nhdh', feature_type = 'flowline')
mapview(lake_2, color="yellow") + mapview(up_streams4, color="yellow") + mapview(lake_head, color="orange") + 
  mapview(lake_3, color="green") + mapview(lake_4, color="purple")+ mapview(lake_5, color="red")
