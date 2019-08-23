#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King July 16, 2018
## Updated: updated Aug 14, 2019 

#------ Step 1: pull predictor values from LAGOS for lakes/reservoirs -------
install.packages("LAGOSNE")
library(LAGOSNE)
#lagosne_get("1.087.3") ### note new version of 1.087.3 LAGOS! didn't work 
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



#------ Step 2: standardize and transform predictor values from LAGOS -------


##------ exploratory analysis -----
hist(fish.data$richness)
boxplot(richness~HU4_ZoneID, data=fish.data)

#conny
plot(fish.data$LakeConnec, fish.data$richness) 
plot(fish.data$wlconnections_area_ha, fish.data$richness) 
plot(fish.data$upstream_lakes_4ha_area_ha, fish.data$richness) 
plot(fish.data$streamdensity_mperha, fish.data$richness)
plot(fish.data$roaddensity_mperha, fish.data$richness)

#morph
plot(fish.data$elevation_m, fish.data$richness) 
plot(fish.data$lakearea_km2, fish.data$richness) 
plot(fish.data$lake_sdf, fish.data$richness) 

#landuse
plot(fish.data$iws_nlcd2006_wet, fish.data$richness) 
plot(fish.data$iws_nlcd2006_agr, fish.data$richness) 
plot(fish.data$iws_nlcd2006_for, fish.data$richness) 
plot(fish.data$iws_nlcd2006_urb, fish.data$richness) 

#HU4
plot(fish.data$HU4_ZoneID, fish.data$richness) 
plot(fish.data$HU4_ZoneID)

#climate 
plot(fish.data$prism_precip_mean, fish.data$richness) 
plot(fish.data$prism_temp_mean, fish.data$richness) 

# correlation exploration
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
my_data <- fish.data[, c(10,11,12,13,14)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
## at the iws level forest is highly negatively correlated with ag and wetland
### at the iws road density and urban are highly correlated 
my_data <- fish.data[, c(23,24,25,26,27)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
## at HUC 4 forest and ag/ wet
## at HUC 4 road density and urban

#### I will get rid of urban and forest for this exercise 

### conny metrics should I keep? 
my_data <- fish.data[, c(15,16,17)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
