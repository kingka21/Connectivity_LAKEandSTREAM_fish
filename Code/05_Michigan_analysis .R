#libraries 
library(rgdal)
library(sf)
library(ggplot2)
library(sp)

#LAGOS shapefile of all lakes in NE 
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/LAGOS_NE_All_Lakes_4ha", layer = "LAGOS_NE_All_Lakes_4ha")
####change to an sf object   
sf_all_lakes<-sf::st_as_sf(lakes_shape)
sf_MIonly<-dplyr::filter(sf_all_lakes, State_Name == "Michigan" )  
#convert it back to a spatial polygons data frame
#MIall.sp<-as(sf_MIonly, "Spatial")
#raster::crs(lakes_shape)
#plot(MIall.sp)

### merge with lat/long to be able to map 
library(LAGOSNE)
lg <- lagosne_load("1.087.1")
lg_points <- data.frame(lagoslakeid=lg$locus$lagoslakeid,
                               nhd_lat=lg$locus$nhd_lat,
                               nhd_long=lg$locus$nhd_long)
lg_points$lagoslakei<-as.factor(lg_points$lagoslakeid)
allMI_lakes<-dplyr::left_join(sf_MIonly, lg_points, by="lagoslakei")

### mapping 
library(ggmap)
library(stringr)
library(devtools)
library(mapdata)
devtools::install_github("dkahle/ggmap")
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
MIonly <- subset(states, region %in% c("michigan")) # and within that select michigan 
p<-ggplot(data = MIonly) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 
#add the points to the map 
p+ geom_point(data=allMI_lakes, aes(x = nhd_long, y = nhd_lat), alpha = "0.5", size = 0.5) +
  geom_point(data=MI_lake_data, aes(x = nhd_long, y = nhd_lat), colour="blue") + 
  xlab("longitude") + ylab("latitude")


########## frequency of lake area to compare biases 
MI_lake_data$lakearea_ha<-MI_lake_data$lakearea_km2*100
par(mfrow=c(1,2))
hist(log(allMI_lakes$Lake_Area_), xlim=c(0,10), ylim=c(0,2000), col="gray", 
     xlab='ln lake area (ha)', ylab='frequency', main='All MI lakes', cex.lab=1.5, cex.main=1.5)
hist(log(MI_lake_data$lakearea_ha), xlim=c(0,10), ylim=c(0,100), col="blue", 
     xlab='ln lake area (ha)', ylab='frequency', main='Sampled MI lakes', cex.lab=1.5, cex.main=1.5)

median(allMI_lakes$Lake_Area_)
median(MI_lake_data$lakearea_ha)



dev.off()
#### histogram of frequency of richness 
hist(MI_lake_data$richness, xlim=c(0,30), ylim=c(0,50), 
     breaks=c(6), col="lightblue",
     xlab="number of species observed", ylab='number of lakes', main = 'observed richness', cex.lab=1.5)
median(MI_lake_data$richness)

### Explore species compostition 
sppinfo<-read.csv("/Users/katelynking/Desktop/MSU Research/Chap 2 Fish/Maggie_Sophie work/Michigan_data_from_2007.csv", header = TRUE)
bearlake<-dplyr::filter(sppinfo, LAKE_CODE == "40_104")
unique(bearlake$SPP_CODE)
sablelake<-dplyr::filter(sppinfo, LAKE_CODE == "2_775")
unique(sablelake$SPP_CODE)

####### HUC4s ######################
#bring in HU4 polygons for visual 
HU4.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU4", layer = "HU4")
names(HU4.poly)
crs(HU4.poly) #shows me the projection of the shapefiles so that I can project the same to the points 

x <- MI_lake_data$nhd_long
y <- MI_lake_data$nhd_lat

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")

MI.aea <- spTransform(lake.ll, prjnew)

plot(HU4.poly)
points(MI.aea)

####change to an sf object   
sf_HU4<-sf::st_as_sf(HU4.poly)

#have to have the zone id in there every time for this to work 
HU4MI<-dplyr::filter(sf_HU4, ZoneID == "HU4_24" | ZoneID == "HU4_31" | ZoneID =="HU4_30"|ZoneID == "HU4_37"| ZoneID =="HU4_24" |ZoneID =="HU4_40" |ZoneID =="HU4_39" |ZoneID =="HU4_32"|ZoneID == "HU4_41" )  
#convert it back to a spatial polygons data frame
HU4MI.sp<-as(HU4MI, "Spatial")
crs(HU4MI.sp)
plot(HU4MI.sp, col=c('polygons'))
points(MI.aea)


####### Regression between richness and area 
area_fit <- lm(richness~log(lakearea_km2), data= MI_lake_data)
summary(area_fit)
## Plot 
ggplot(data = MI_lake_data, (aes(x = log(lakearea_km2), y = richness ))) + geom_point() +
  geom_smooth(method='lm') +  #second row and 4th column 
  annotate("text", label = "p<0.01", x = -2, y = 27, size = 4, colour = "black") + 
  xlab("ln lake area (km2)") + theme_bw()

############################################
### Graphs of lm for each HU4 overlaid
############################################
unique(MI_lake_data$HU4_ZoneID)
##subset MI_lake_data
HU_31 <- subset(MI_lake_data, HU4_ZoneID == "HU4_31") 
HU_30 <- subset(MI_lake_data, HU4_ZoneID == "HU4_30") 
HU_37 <- subset(MI_lake_data, HU4_ZoneID == "HU4_37") 
HU_24 <- subset(MI_lake_data, HU4_ZoneID == "HU4_24") 
HU_40 <- subset(MI_lake_data, HU4_ZoneID == "HU4_40")
HU_39 <- subset(MI_lake_data, HU4_ZoneID == "HU4_39") 
HU_32 <- subset(MI_lake_data, HU4_ZoneID == "HU4_32") 
HU_41 <- subset(MI_lake_data, HU4_ZoneID == "HU4_41")



## Plot lm for every HUC for evenness and RUE_chla
ggplot(data = MI_lake_data, (aes(x = richness, y = log(lakearea_km2)))) +
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_31'), method='lm', se = FALSE, aes(col = 'HU4_31')) + 
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_30'), method='lm', se = FALSE, aes(col = 'HU4_30')) +
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_37'), method='lm', se = FALSE, aes(col = 'HU4_37')) + 
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_24'), method='lm', se = FALSE, aes(col = 'HU4_24')) +
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_40'), method='lm', se = FALSE, aes(col = 'HU4_40')) + 
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_39'), method='lm', se = FALSE, aes(col = 'HU4_39')) +
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_32'), method='lm', se = FALSE, aes(col = 'HU4_32')) + 
  geom_smooth(data = subset(MI_lake_data, HU4_ZoneID == 'HU4_41'), method='lm', se = FALSE, aes(col = 'HU4_41')) +
  scale_colour_manual(values=c("HU4_31" = 'blue', 'HU4_30' = 'red', 'HU4_37' = 'yellow', 'HU4_24' = 'green', 'HU4_40' = 'purple', 'HU4_39' = 'black', 'HU4_32' = 'orange', 'HU4_41' = 'turquoise')) +
  labs(title = sprintf('Richness vs. Area')) 




######## boxplots
boxplot(hu4_nlcd2006_agr ~ HU4_ZoneID,
        data=MI_lake_data
)

boxplot(iws_nlcd2006_agr ~ HU4_ZoneID,
        data=MI_lake_data
)

plot(MI_lake_data$richness~MI_lake_data$iws_nlcd2006_agr)