#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King July 16, 2018
## Updated: 

#Question 

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
