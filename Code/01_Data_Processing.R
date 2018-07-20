#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King July 15, 2018
## Updated: 

#load libraries 
#install.packages('rgdal')
library(rgdal)
library(sp)
library(dplyr)
library(raster)

#------ Step 1: Merge fish data with LAGOS -------

# Read SHAPEFILE.shp from the LAGOS database 
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/LAGOS_NE_All_Lakes_4ha", layer = "LAGOS_NE_All_Lakes_4ha")

# Michigan fish data from 2007 
MIdata <- read.csv("/Users/katelynking/Desktop/MSU Research/Chap 2 Fish/Maggie_Sophie work/Michigan_data_from_2007.csv", header = TRUE)
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

# bring in expanded depth dataset
#expanded_depth <- read.csv("data/lakesnodepth_6.15.18 - lakesnodepth_27Jun17-EDIT.csv",
 #                          stringsAsFactors = FALSE)
#expanded_depth <- select(expanded_depth, lagoslakeid, ex_maxdepth_m = NEW.maxdepth..meters.)

#depth <- dplyr::left_join(depth, expanded_depth, by = "lagoslakeid")
#depth$maxdepth_m[is.na(depth$maxdepth_m) & !is.na(depth$ex_maxdepth_m)] <-
 # depth$ex_maxdepth_m[is.na(depth$maxdepth_m) & !is.na(depth$ex_maxdepth_m)]
#depth <- dplyr::select(depth, -ex_maxdepth_m)

#lake area, perim, sdf, lat, long, elevation, and all ids
lake_morphometry <- data.frame(lagoslakeid=lg$locus$lagoslakeid,
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

## Merge LAGOS to MI lakes (96 lakes but 4 of them didn't match LAGOS)
local_preds$lagoslakei<-as.factor(local_preds$lagoslakeid)
fish.local<-left_join(fish.LAGOS, local_preds, by = 'lagoslakei')

MI.fish.LAGOS<-left_join(fish.local, hu4_preds, by = 'HU4_ZoneID')

#get rid of NAs
MI.fish.LAGOS<-MI.fish.LAGOS[!(is.na(MI.fish.LAGOS$ComID)),]

##pull out only columns for initial model development 
fish.data<-dplyr::select(MI.fish.LAGOS, lakearea_km2, elevation_m, LakeConnec, lagoslakei, 
                         HU4_ZoneID, richness, nhd_lat, nhd_long, lake_sdf, 
                         iws_nlcd2006_wet, iws_nlcd2006_agr, iws_nlcd2006_for, iws_nlcd2006_urb, iws_roaddensity_mperha,
                         iws_upstream_lakes_4ha_area_ha, iws_wlconnections_area_ha, iws_streamdensity_mperha,
                         prism_precip_mean, prism_temp_mean, 
                         hu4_streamdensity_mperha,hu4_lakes_area_ha,hu4_wetlands_area_ha, 
                         hu4_nlcd2006_urb, hu4_nlcd2006_for, hu4_nlcd2006_agr, hu4_nlcd2006_wet, hu4_roaddensity_mperha)

write.csv(fish.data, "Data/MI_lake_data.csv", row.names = FALSE)

fish.data<-read.csv('Data/MI_lake_data.csv')

#------ Step 4: standardize and transform predictor values from LAGOS -------
