#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King July 15, 2018
## Updated: 

#load libraries 
#install.packages('rgdal')
library(rgdal)
library(sp)
library(dplyr)
library(raster)
library(ggplot2)

#------ Step 1: Merge fish data with LAGOS -------

# Read SHAPEFILE.shp from the LAGOS database 
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/LAGOS_NE_All_Lakes_4ha", layer = "LAGOS_NE_All_Lakes_4ha")

# Michigan fish data from MI 
MIdata <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/Maggie_Sophie work/Michigan_data_from_2007.csv", header = TRUE)
MIpoints<-dplyr::select(MIdata, LAKE_CODE, LAT_DD, LONG_DD)
MIpoints<- MIpoints[!duplicated(paste(MIpoints$LAKE_CODE)),] #select only one row of each lake
MIpoints<-MIpoints[!(is.na(MIpoints$LAT_DD)),] #get rid of two lakes that don't have lat/long
x <- MIpoints$LONG_DD
y <- MIpoints$LAT_DD
crs(lakes_shape) #shows me the projection of the shapefiles so that I can project the same to the points 

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")

MI.aea <- spTransform(lake.ll, prjnew)
plot(MI.aea)

#use the over function to match points to LAGOS polygons. 
#Returns a data.frame of the second argument with row entries corresponding to the first argument
only.MI.fish<-sp::over(MI.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
only.MI.fish$LAKE_CODE<-paste(MIpoints$LAKE_CODE)  

#investigate lakes that did not have matches n= 4
no.match<-only.MI.fish[is.na(only.MI.fish$ComID),] 
no.matchlakes<-filter(MIpoints, LAKE_CODE == "48_621" | LAKE_CODE == "63_2" | LAKE_CODE == "48_485" | LAKE_CODE == "48_622")
x <- no.matchlakes$LONG_DD
y <- no.matchlakes$LAT_DD
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")
nomatch.aea <- spTransform(lake.ll, prjnew)
library(mapview)
mapView(nomatch.aea)

#use the hydrolinks package to link the lakes to NHD hires
library(hydrolinks)
linked_lakes<-link_to_waterbodies(no.matchlakes$LAT_DD, no.matchlakes$LONG_DD, no.matchlakes$LAKE_CODE, "nhdh")
unlinked<-subset(no.matchlakes, !(LAKE_CODE %in% linked_lakes$MATCH_ID))

#try to match the unlinked (this point pops up right at the river mouth and lake )
linked_2 = link_to_waterbodies(unlinked$LAT_DD, unlinked$LONG_DD, unlinked$LAKE_CODE, dataset = 'nhdh', buffer = 30)

#stack the two sets 
linked_lakes<-gtools::smartbind(linked_2, linked_lakes)

#link the nhdh perm identifiers to LAGOS iws_nhhdid ## 3 of the lakes are less than 4 ha so we don't have LAGOS data for them. 
lagosinfo<-data.frame(lg$iws)
newlagosjoin<-left_join(linked_lakes, lagosinfo, by=c("permanent_"="iws_nhdid"))
only.MI.fish<-rename(only.MI.fish, permanent_ = Permanent_)
only.MI.fish<-rename(only.MI.fish, lagoslakeid = lagoslakei)
newlagosjoin<-rename(newlagosjoin, LAKE_CODE = MATCH_ID)

unlinkedcodes<-dplyr::select(newlagosjoin, permanent_, lagoslakeid, LAKE_CODE)
datacodes<-dplyr::select(only.MI.fish, permanent_, lagoslakeid, LAKE_CODE)
unlinkedcodes$permanent_<-as.factor(unlinkedcodes$permanent_)
unlinkedcodes$lagoslakeid<-as.factor(unlinkedcodes$lagoslakeid)

only.MI.fish<-gtools::smartbind(unlinkedcodes, datacodes)

#get rid of NAs (93 lakes left) 
only.MI.fish<-only.MI.fish[!(is.na(only.MI.fish$lagoslakeid)),]

#------ Step 2: calculate fish species richness -------

MI<-dplyr::select(MIdata, Lake_Name, LAKE_CODE, GEAR, SPP_CODE, Survey_Effort_Key, Total_Number_Caught)
diversity<-with(MI, tapply(SPP_CODE, LAKE_CODE, FUN = function(x) length(unique(x))))
diversity<-as.data.frame(diversity)
colnames(diversity) <- c("richness")
diversity$LAKE_CODE<-row.names(diversity)  #get the lakes name to show up as a column

#join 
fish.LAGOS<-left_join(only.MI.fish, diversity, by=c('LAKE_CODE'))

#------ Step 3: add possible predictor values from LAGOS -------
#install.packages("LAGOSNE")
library(LAGOSNE)
lg <- lagosne_load("1.087.1")

#watershed scale land use/cover
IWS_LULC <- lg$iws.lulc
IWS_LULC <- data.frame(lagoslakeid=IWS_LULC$lagoslakeid,
                       iws_openwater2006_pct=IWS_LULC$iws_nlcd2006_pct_11,
                       iws_developed_open2006_pct=IWS_LULC$iws_nlcd2006_pct_21,
                       iws_developed_low2006_pct=IWS_LULC$iws_nlcd2006_pct_22,
                       iws_developed_med2006_pct=IWS_LULC$iws_nlcd2006_pct_23,
                       iws_developed_high2006_pct=IWS_LULC$iws_nlcd2006_pct_24,
                       iws_deciduous2006_pct=IWS_LULC$iws_nlcd2006_pct_41,
                       iws_evergreen2006_pct=IWS_LULC$iws_nlcd2006_pct_42,
                       iws_mixedforest2006_pct=IWS_LULC$iws_nlcd2006_pct_43,
                       iws_pasture_hay2006_pct=IWS_LULC$iws_nlcd2006_pct_81,
                       iws_rowcrops2006_pct=IWS_LULC$iws_nlcd2006_pct_82,
                       iws_roaddensity_mperha=IWS_LULC$iws_roaddensity_density_mperha,
                       iws_wetland_woody2006_pct=IWS_LULC$iws_nlcd2006_pct_90,
                       iws_wetland_emergent2006_pct=IWS_LULC$iws_nlcd2006_pct_95)

IWS_LULC$iws_nlcd2006_urb <- IWS_LULC$iws_developed_open2006_pct +
  IWS_LULC$iws_developed_low2006_pct + IWS_LULC$iws_developed_med2006_pct +
  IWS_LULC$iws_developed_high2006_pct

IWS_LULC$iws_nlcd2006_for <- IWS_LULC$iws_deciduous2006_pct +
  IWS_LULC$iws_evergreen2006_pct + IWS_LULC$iws_mixedforest2006_pct

IWS_LULC$iws_nlcd2006_agr <- IWS_LULC$iws_pasture_hay2006_pct +
  IWS_LULC$iws_rowcrops2006_pct

IWS_LULC$iws_nlcd2006_wet <- IWS_LULC$iws_wetland_woody2006_pct +
  IWS_LULC$iws_wetland_emergent2006_pct

# lake morphometry and connectivity
# max and mean depth
depth <- data.frame(lagoslakeid=lg$lakes_limno$lagoslakeid,
                    maxdepth_m=lg$lakes_limno$maxdepth,
                    meandepth_m=lg$lakes_limno$meandepth)

#lake area, perim, sdf, lat, long, elevation, and all ids
lake_morphometry <- data.frame(lagoslakeid=lg$locus$lagoslakeid,
                               HU4_ZoneID=lg$locus$hu4_zoneid,
                               HU6_ZoneID=lg$locus$hu6_zoneid,
                               HU8_ZoneID=lg$locus$hu8_zoneid,
                               HU12_ZoneID=lg$locus$hu12_zoneid,
                               nhd_lat=lg$locus$nhd_lat,
                               nhd_long=lg$locus$nhd_long,
                               lakearea_ha=lg$locus$lake_area_ha,
                               lakeperim_m=lg$locus$lake_perim_meters,
                               elevation_m=lg$locus$ elevation_m)

#calculate shoreline development: convert lake area to km2; convert lake perim to km
#lake_sdf = lake_perim_km / (2 * (pi * lake_area_km2)^1/2
lake_morphometry$lakearea_km2 <- lake_morphometry$lakearea_ha / 100
lake_morphometry$lakeperim_km <- lake_morphometry$lakeperim_m / 1000
lake_morphometry$lake_sdf     <- lake_morphometry$lakeperim_km/(2 *
                                                                  (pi*lake_morphometry$lakearea_km2)^0.5)


# iws connectivity
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        glaciation=lake_conn$latewisconsinglaciation_glacial,
                        conn_class=lake_conn$lakeconnection,
                        iws_upstream_lakes_4ha_area_ha=lake_conn$upstream_lakes_4ha_area_ha,
                        iws_wlconnections_area_ha=lake_conn$wlconnections_allwetlands_contributing_area_ha)

IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid,
                       iws_streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha)
#### HU4 variables 
#climate 
HU4_climate <- lg$hu4.chag
HU4_climate <- data.frame(HU4_ZoneID=HU4_climate$hu4_zoneid,
                          prism_precip_mean=HU4_climate$hu4_prism_ppt_30yr_normal_800mm2_annual_mean,
                          prism_temp_mean=HU4_climate$hu4_prism_tmean_30yr_normal_800mm2_annual_mean)

HU4_LULC <- lg$hu4.lulc
HU4_LULC <- data.frame(HU4_ZoneID=HU4_LULC$hu4_zoneid,
                       developed_open2006_pct=HU4_LULC$hu4_nlcd2006_pct_21,
                       developed_low2006_pct=HU4_LULC$hu4_nlcd2006_pct_22,
                       developed_med2006_pct=HU4_LULC$hu4_nlcd2006_pct_23,
                       developed_high2006_pct=HU4_LULC$hu4_nlcd2006_pct_24,
                       deciduous2006_pct=HU4_LULC$hu4_nlcd2006_pct_41,
                       evergreen2006_pct=HU4_LULC$hu4_nlcd2006_pct_42,
                       mixedforest2006_pct=HU4_LULC$hu4_nlcd2006_pct_43,
                       pasture_hay2006_pct=HU4_LULC$hu4_nlcd2006_pct_81,
                       rowcrops2006_pct=HU4_LULC$hu4_nlcd2006_pct_82,
                       hu4_roaddensity_mperha=HU4_LULC$hu4_roaddensity_density_mperha,
                       wetland_woody2006_pct=HU4_LULC$hu4_nlcd2006_pct_90,
                       wetland_emergent2006_pct=HU4_LULC$hu4_nlcd2006_pct_95)

HU4_LULC$hu4_nlcd2006_urb <- HU4_LULC$developed_open2006_pct +
  HU4_LULC$developed_low2006_pct + HU4_LULC$developed_med2006_pct +
  HU4_LULC$developed_high2006_pct

HU4_LULC$hu4_nlcd2006_for <- HU4_LULC$deciduous2006_pct +
  HU4_LULC$evergreen2006_pct + HU4_LULC$mixedforest2006_pct

HU4_LULC$hu4_nlcd2006_agr <- HU4_LULC$pasture_hay2006_pct +
  HU4_LULC$rowcrops2006_pct

HU4_LULC$hu4_nlcd2006_wet <- HU4_LULC$wetland_woody2006_pct +
  HU4_LULC$wetland_emergent2006_pct

# HU4 connectivity
HU4_conn <- lg$hu4.conn
HU4_conn <- data.frame(HU4_ZoneID=HU4_conn$hu4_zoneid,
                       hu4_streamdensity_mperha=HU4_conn$hu4_streamdensity_streams_density_mperha,
                       hu4_lakes_area_ha=HU4_conn$hu4_lakes_overlapping_area_ha,
                       hu4_wetlands_area_ha=HU4_conn$hu4_wl_allwetlandsdissolved_overlapping_area_ha)


#merge all local predictors
local_preds <- left_join(lake_morphometry, depth, by = "lagoslakeid") %>%
  left_join(IWS_LULC, by = "lagoslakeid") %>%
  left_join(lake_conn, by = "lagoslakeid") %>%
  left_join(IWS_conn, by = "lagoslakeid")

#merge all HU4 predictors 
hu4_preds <- left_join(HU4_climate, HU4_conn, by = "HU4_ZoneID") %>%
  left_join(HU4_LULC, by = "HU4_ZoneID") 

## Merge LAGOS to MI lakes 
local_preds$lagoslakeid<-as.factor(local_preds$lagoslakeid)
fish.local<-left_join(fish.LAGOS, local_preds, by = 'lagoslakeid')

MI.fish.LAGOS<-left_join(fish.local, hu4_preds, by = 'HU4_ZoneID')

#plot to make sure join was correct
ggplot() + 
  geom_point(data=MI.fish.LAGOS, aes(x=nhd_long, y=nhd_lat))

#add reservoir and natural lake data 
#lakes<- read.csv("/Users/katelynking/Desktop/mi_nl_ll.csv", header=TRUE)

#newfish<-dplyr::left_join(MI.fish.LAGOS, lakes, by= "lagoslakeid")


##pull out only columns for initial model development 
#fish.data<-dplyr::select(MI.fish.LAGOS, lakearea_km2, elevation_m, LakeConnec, lagoslakei, 
 #                        HU4_ZoneID, richness, nhd_lat, nhd_long, lake_sdf, 
  #                       iws_nlcd2006_wet, iws_nlcd2006_agr, iws_nlcd2006_for, iws_nlcd2006_urb, iws_roaddensity_mperha,
   #                      iws_upstream_lakes_4ha_area_ha, iws_wlconnections_area_ha, iws_streamdensity_mperha,
    #                     prism_precip_mean, prism_temp_mean, 
     #                    hu4_streamdensity_mperha,hu4_lakes_area_ha,hu4_wetlands_area_ha, 
      #                   hu4_nlcd2006_urb, hu4_nlcd2006_for, hu4_nlcd2006_agr, hu4_nlcd2006_wet, hu4_roaddensity_mperha)

write.csv(MI.fish.LAGOS, "Data/MI_lake_data.csv", row.names = FALSE)

#fish.data<-read.csv('Data/MI_lake_data.csv')

#------ Step 4: calculate beta diversity for HUC4s
library(vegan)
#filter to just columns we need
MIdata1<-dplyr::select(MIdata, LAKE_CODE, SPP_CODE, Total_Number_Caught) 

#group by lake and species to add up all the species for a lake
MIdata2 <-MIdata1 %>% group_by(LAKE_CODE, SPP_CODE) %>% summarise(total=sum(Total_Number_Caught))
MIdata3<-tidyr::spread(MIdata2, SPP_CODE, total)

HUC_codes<-dplyr::select(MI.fish.LAGOS, LAKE_CODE, lagoslakeid, HU4_ZoneID, HU6_ZoneID)
MIdata4<-dplyr::left_join(MIdata3, HUC_codes)
MIdata4[is.na(MIdata4)] <- 0 #NAs to 0 
MIdata4<-MIdata4[!(is.na(MIdata4$HU6_ZoneID)),] #get rid of 5 lakes that didn't match to LAGOS

### create a function to calculate beta diversity average for a region
 calcbeta<-function(x) { # x = a dataframe   
   new<- subset(x, select = -c(LAKE_CODE, lagoslakeid, HU4_ZoneID, HU12_ZoneID))
   #dissimilarity
   dis<-vegdist(new, "bray", binary = TRUE) 
   #average dissimilarity
   avg<-mean(dis)
   return(avg)
 }
 
# loop to run dissimilarity on all HUC 4s
HUC_string <-as.character(unique(MIdata4$HU4_ZoneID))

output_df <- data.frame(HU4_ZoneID=NA, beta=NA)            
for (i in 1:8) {
  sub<-subset(MIdata4, HU4_ZoneID == HUC_string[i] )
  beta<-calcbeta(sub)
  output_df[i,1]=HUC_string[i]
  output_df[i,2]=beta
} 

### loop to run on all HUC12s 
HUC_string <-as.character(unique(MIdata4$HU12_ZoneID))

output_df <- data.frame(HU6_ZoneID=NA, beta=NA)            
for (i in 1:10) {
  sub<-subset(MIdata4, HU6_ZoneID == HUC_string[i] )
  beta<-calcbeta(sub)
  output_df[i,1]=HUC_string[i]
  output_df[i,2]=beta
} 


#join the beta outputs 
MI.fish_beta <-left_join(MI.fish.LAGOS, output_df, by = 'HU6_ZoneID')

## compare alpha and beta diversity (Pool et al 2014)
ggplot(data = MI.fish_beta, (aes(x = richness, y = beta ))) + geom_point() +
  geom_smooth(method='lm') 

## bar graph of the disimilarity values within each HUC to compare distribution (left skewed in the Roden 2018 paper)

## making maps 
#base map of michigan 
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
MImap <- subset(states, region %in% c("michigan")) # and within that select michigan 
p<-ggplot(data = MImap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

#map of beta diversity at the HUC4 scale 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
   labs(color="beta diversity")

#map of beta diversity at the HUC6 scale 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
  labs(color="beta diversity")

#map of alpha diversity 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(richness))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")

####### HUC4s ######################
#bring in HU4 polygons for visual 
HU4.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU4", layer = "HU4")
crs(HU4.poly) #shows me the projection of the shapefiles so that I can project the same to the points 

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")

####change to an sf object   
sf_HU4<-sf::st_as_sf(HU4.poly)

#have to have the zone id in there every time for this to work 
HU4MI<-dplyr::filter(sf_HU4, ZoneID == "HU4_24" | ZoneID == "HU4_31" | ZoneID =="HU4_30"|ZoneID == "HU4_37"|ZoneID =="HU4_40" |ZoneID =="HU4_39" |ZoneID =="HU4_32"|ZoneID == "HU4_41" )  
#convert it back to a spatial polygons data frame
HU4MI.sp<-as(HU4MI, "Spatial")
crs(HU4MI.sp)
plot(HU4MI.sp)

ggplot(data = HU4MI.sp) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = factor(ZoneID), color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

## crop one polygon to another extent 
library(raster) 
MImap.sp<- SpatialPolygons(MImap, pO=MImap$order, proj4string=prjnew) 

MImap.sp<-spTransform(MImap, prjnew)
out <- crop(HU4MI.sp, MImap)

#Marion et al. 2017: If we denote the number of species shared between two sites as a and the numbers of unique species (not shared) as b and c, 
#then S = a + b + c and α = (2 a + b + c)/2 so that β_w = (b+c)/(2 a + b + c).
#This is the Sørensen dissimilarity as defined in vegan function vegdist with argument binary = TRUE. 
## example ways of calculating the same Sørensen dissimilarity -- gives the same answer.
#1 means that the communities are totally different. 
data(BCI)
d0 <- vegdist(BCI, "bray", binary = TRUE)  # binary: (A+B-2*J)/(A+B)
d2 <- designdist(BCI, "(b+c)/(2*a+b+c)", abcd = TRUE)


#------ Step 5: standardize and transform predictor values from LAGOS -------

