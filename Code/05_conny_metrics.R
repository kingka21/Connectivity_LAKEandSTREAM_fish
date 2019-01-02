#downstream and upstream lake areas #Note: Match_ID is perm ID from highres, lost some lakes 
linked_nhdplusv2 = link_to_waterbodies(only.MI.fish$nhd_lat, only.MI.fish$nhd_long,  only.MI.fish$permanent_, dataset = 'nhdplusv2', buffer = 30)

## loop to traverse upstream metrics for all lakes 
output <- data.frame(permanent_=NA, up_lakes_area=NA, up_streams_length=NA)            
for (i in 1:length(lake_poly)) {
  # traverse upstream
  upstream = traverse_flowlines(Inf, lake_poly[i]$permanent_, direction = 'in')
  upstream_streams = get_shape_by_id(upstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  if(is.na(upstream_lakes)) {0}   
  if(is.na(upstream_streams)) {0}  
  #return upstream stream length 
  up_streams_sum<-sum(upstream_streams$lengthkm)
  #return upstream lake area (sqkm) 
  up_lakes_sum<-sum(upstream_lakes$areasqkm) 
 
  
  output[i,1]=lake_poly[i]
  output[i,2]=up_lakes_sum
  output[i,3]=up_streams_sum
} 



#traverse downstream 
network = traverse_flowlines(Inf, sf_duck$permanent_, direction = 'out')
### CROP OUT GREAT LAKES 
#traverse back upstream from the terminal reach. This will get all of the water bodies  
return_network = traverse_flowlines(Inf, network[nrow(network),1], direction='in')
connected_streams = get_shape_by_id(return_network$permanent_, dataset = 'nhdh', feature_type = 'flowline')
connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')
#return downstream stream length 
sum(connected_streams$lengthkm) - sum(upstream_streams$lengthkm)
#return downstream lake area (sqkm) *need to subtract GLs somehow 
sum(connected_lakes$areasqkm) - sum(upstream_lakes$areasqkm) - sf_duck$areasqkm

upstreams_sp<-sf::as_Spatial(upstream_streams)
streams_sp<-sf::as_Spatial(connected_streams)
lakes_sp<-sf::as_Spatial(connected_lakes)
mapview(connected_streams) + mapview(connected_lakes) 

#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Connectivity metrics that are already in LAGOS 
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
