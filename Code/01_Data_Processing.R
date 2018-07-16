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
MIpoints<-select(MIdata, LAKE_CODE, LAT_DD, LONG_DD)
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
                       openwater2006_pct=IWS_LULC$iws_nlcd2006_pct_11,
                       developed_open2006_pct=IWS_LULC$iws_nlcd2006_pct_21,
                       developed_low2006_pct=IWS_LULC$iws_nlcd2006_pct_22,
                       developed_med2006_pct=IWS_LULC$iws_nlcd2006_pct_23,
                       developed_high2006_pct=IWS_LULC$iws_nlcd2006_pct_24,
                       deciduous2006_pct=IWS_LULC$iws_nlcd2006_pct_41,
                       evergreen2006_pct=IWS_LULC$iws_nlcd2006_pct_42,
                       mixedforest2006_pct=IWS_LULC$iws_nlcd2006_pct_43,
                       pasture_hay2006_pct=IWS_LULC$iws_nlcd2006_pct_81,
                       rowcrops2006_pct=IWS_LULC$iws_nlcd2006_pct_82,
                       roaddensity_mperha=IWS_LULC$iws_roaddensity_density_mperha,
                       wetland_woody2006_pct=IWS_LULC$iws_nlcd2006_pct_90,
                       wetland_emergent2006_pct=IWS_LULC$iws_nlcd2006_pct_95)

IWS_LULC$iws_nlcd2006_urb <- IWS_LULC$developed_open2006_pct +
  IWS_LULC$developed_low2006_pct + IWS_LULC$developed_med2006_pct +
  IWS_LULC$developed_high2006_pct

IWS_LULC$iws_nlcd2006_for <- IWS_LULC$deciduous2006_pct +
  IWS_LULC$evergreen2006_pct + IWS_LULC$mixedforest2006_pct

IWS_LULC$iws_nlcd2006_agr <- IWS_LULC$pasture_hay2006_pct +
  IWS_LULC$rowcrops2006_pct

IWS_LULC$iws_nlcd2006_wet <- IWS_LULC$wetland_woody2006_pct +
  IWS_LULC$wetland_emergent2006_pct

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


# connectivity
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        glaciation=lake_conn$latewisconsinglaciation_glacial,
                        conn_class=lake_conn$lakeconnection,
                        upstream_lakes_4ha_area_ha=lake_conn$upstream_lakes_4ha_area_ha,
                        wlconnections_area_ha=lake_conn$wlconnections_allwetlands_contributing_area_ha)

lake_conn$lakeconn_v2<-sapply(lake_conn$conn_class, function(x) {
  if(x == 'DR_LakeStream') {'DR_LakeStream'}
  else {
    if(x == 'Headwater') {'HW_ISO'}
    else {
      if(x == 'DR_Stream') {'DR_Stream'}
      else {
        if(x== 'Isolated') {'HW_ISO'}
      }}}})

IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid,
                       streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha)

#merge all local predictors
local_preds <- left_join(lake_morphometry, depth, by = "lagoslakeid") %>%
  left_join(IWS_LULC, by = "lagoslakeid") %>%
  left_join(lake_conn, by = "lagoslakeid") %>%
  left_join(IWS_conn, by = "lagoslakeid")

## Merge LAGOS to MI lakes (96 lakes but 4 of them didn't match LAGOS)
local_preds$lagoslakei<-as.factor(local_preds$lagoslakeid)
MI.fish.LAGOS<-left_join(fish.LAGOS, local_preds, by = 'lagoslakei')
