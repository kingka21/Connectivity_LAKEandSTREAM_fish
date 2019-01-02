#load libraries 
library(dplyr)
library(hydrolinks)
library(sf)
library(mapview)
library(LAGOSNE)

####### Get downstream and upstream lake areas ###### 

#Note: Match_ID is perm ID from highres, lost some lakes if linking to nhdPlus
    #linked_nhdplusv2 = link_to_waterbodies(only.MI.fish$nhd_lat, only.MI.fish$nhd_long,  only.MI.fish$permanent_, dataset = 'nhdplusv2', buffer = 30)

#get shapefiles from nhd high-res by using the permanent identifiers from LAGOS #lost one lake  
lake_poly = get_shape_by_id(only.MI.fish$permanent_, dataset = 'nhdh', feature_type = 'waterbody')

## loop to traverse upstream metrics for all lakes 
## for all the lakes 
output <- data.frame(permanent_=NA, up_lakes_area=NA, up_streams_length=NA)  
for (i in 1:length(lake_poly)) {
  # traverse upstream
  upstream = traverse_flowlines(Inf, lake_poly[i]$permanent_, direction = 'in')
  upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  up_lakes_sum <- if(is.na(upstream_lakes)) {0}   
  else{ sum(upstream_lakes$areasqkm) #return upstream lake area (sqkm) 
  }
  up_streams_sum <-if(is.na(upstream_streams)) {0}  
  else{ sum(upstream_streams$lengthkm) #return upstream lake area (sqkm) 
  }
  
  output[i,1]=lake_poly[i]$permanent_
  output[i,2]=up_lakes_sum
  output[i,3]=up_streams_sum
} 

## test one lake Duck Lake connected to Lake Superior 
test<-filter(lake_poly, permanent_ == 123397648)

output <- data.frame(permanent_=NA, up_lakes_area=NA, up_streams_length=NA, down_lakes_area=NA, down_streams_length=NA)            
for (permanent_ in test) {
  # traverse upstream
  upstream = traverse_flowlines(Inf, test$permanent_, direction = 'in')
  upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  up_lakes_sum <- if(is.na(upstream_lakes)) {0}   
                      else{ sum(upstream_lakes$areasqkm) #return upstream lake area (sqkm) 
                      }
  up_streams_sum <-if(is.na(upstream_streams)) {0}  
                     else{ sum(upstream_streams$lengthkm) #return upstream lake area (sqkm) 
                     }

  #traverse downstream 
  network = traverse_flowlines(Inf, test$permanent_, direction = 'out')
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
  
  down_lakes_sum <- sum(connected_lakes$areasqkm) - up_lakes_sum - test$areasqkm #return downstream lake area (sqkm)= all lakes - upstream lakes - the focal lake area
  
  down_streams_sum <- sum(connected_streams$lengthkm) - up_streams_sum #return downstream stream length
  
  
  output[,1]=test$permanent_
  output[,2]=up_lakes_sum
  output[,3]=up_streams_sum
  output[,4]=down_lakes_sum
  output[,5]=down_streams_sum
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
