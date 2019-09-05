#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Fall 2018
## Updated: updated Aug 14, 2019 

#load libraries
library(vegan)
library(dplyr)
library(ggplot2)

#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/streamsall.csv')
order<-dplyr::select(stream_order_info, COMIDv2_JR, order_class)
stream_dat<-left_join(stream_dat, order)

#------ calculate beta diversity for HUC4s -------
#filter to just columns we need and rename them 
dataL<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, LAT_DD, LON_DD) 
dataS<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, order_class, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, lat, lon) 
setnames(dataS, old=c("COMIDv2_JR", "ind_count", 'order_class', "lat", "lon"), new=c("SITE_ID", "TOTAL_COUNT", "CLASS", "LAT_DD", "LON_DD"))
setnames(dataL, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))

ISO<-filter(dataL, CLASS == "Isolated")
HW_L<-filter(dataL, CLASS == "Headwater")
DR_ST<-filter(dataL, CLASS== "DR_Stream")
DR_STLK<-filter(dataL, CLASS == "DR_LakeStream")
HW<-filter(dataS, CLASS == "HW")
MID<-filter(dataS, CLASS == "MID")
RIVER<-filter(dataS, CLASS == "RIVER")

#create a function to reformat each dataset 
reform<-function(x) { # x = a dataframe   
  dat <- x %>% group_by(SITE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT)) #group by eco and species 
  dat2<-tidyr::spread(dat, SPP_CODE_new, total) #reformat the data table 
  dat2[is.na(dat2)] <- 0 #NAs to 0 
  codes<-dplyr::select(x, SITE_ID, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, LAT_DD, LON_DD) #need to join ID and codes back to table
  codes2<-codes[!duplicated(paste(codes$SITE_ID)),]
  dat3<-dplyr::left_join(dat2, codes2, by = "SITE_ID")
  return(dat3)
}

ISO2<-reform(ISO)
HW_L2<-reform(HW_L)
DR_ST2<-reform(DR_ST)
DR_STLK2<-reform(DR_STLK)
HW2<-reform(HW)
MID2<-reform(MID)
RIVER2<-reform(RIVER)

summary(ISO2$HU4_ZoneID) #how many lake in each HUC 
summary(HW_L2$HU4_ZoneID) #how many res in each HUC 
summary(DR_ST2$HU4_ZoneID) #how many stream in each HUC 

### create a function to calculate beta diversity average for a region
calcbeta<-function(x) { # x = a dataframe   
  new<- subset(x, select = -c(SITE_ID, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, LAT_DD, LON_DD))
  #dissimilarity
  dis<-vegdist(new, "bray", binary = TRUE) 
  #average dissimilarity
  avg<-mean(dis)
  return(avg)
}

# ISOLATED LAKES: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(ISO2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(ISO2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(ISO2$HU12_ZoneID)) 

ISO_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
ISO_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
ISO_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:19) {
  sub<-subset(ISO2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  ISO_beta_4[i,1]=HUC4_string[i]
  ISO_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:46) {
  sub<-subset(ISO2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  ISO_beta_8[i,1]=HUC8_string[i]
  ISO_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:87) {
  sub<-subset(ISO2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  ISO_beta_12[i,1]=HUC12_string[i]
  ISO_beta_12[i,2]=beta
} 

# HW LAKES: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(HW_L2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(HW_L2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(HW_L2$HU12_ZoneID)) 

HWL_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
HWL_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
HWL_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:22) {
  sub<-subset(HW_L2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  HWL_beta_4[i,1]=HUC4_string[i]
  HWL_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:42) {
  sub<-subset(HW_L2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  HWL_beta_8[i,1]=HUC8_string[i]
  HWL_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:70) {
  sub<-subset(HW_L2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  HWL_beta_12[i,1]=HUC12_string[i]
  HWL_beta_12[i,2]=beta
} 

# DR_st LAKES: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(DR_ST2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(DR_ST2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(DR_ST2$HU12_ZoneID)) 

DRL_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
DRL_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
DRL_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:28) {
  sub<-subset(DR_ST2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  DRL_beta_4[i,1]=HUC4_string[i]
  DRL_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:99) {
  sub<-subset(DR_ST2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  DRL_beta_8[i,1]=HUC8_string[i]
  DRL_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:188) {
  sub<-subset(DR_ST2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  DRL_beta_12[i,1]=HUC12_string[i]
  DRL_beta_12[i,2]=beta
} 

# DR_stlk LAKES: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(DR_STLK2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(DR_STLK2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(DR_STLK2$HU12_ZoneID)) 

DRLL_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
DRLL_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
DRLL_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:27) {
  sub<-subset(DR_STLK2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  DRLL_beta_4[i,1]=HUC4_string[i]
  DRLL_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:75) {
  sub<-subset(DR_STLK2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  DRLL_beta_8[i,1]=HUC8_string[i]
  DRLL_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:193) {
  sub<-subset(DR_STLK2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  DRLL_beta_12[i,1]=HUC12_string[i]
  DRLL_beta_12[i,2]=beta
} 

# HW STREAMS: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(HW2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(HW2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(HW2$HU12_ZoneID)) 

HW2_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
HW2_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
HW2_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:27) {
  sub<-subset(HW2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  HW2_beta_4[i,1]=HUC4_string[i]
  HW2_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:113) {
  sub<-subset(HW2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  HW2_beta_8[i,1]=HUC8_string[i]
  HW2_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:682) {
  sub<-subset(HW2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  HW2_beta_12[i,1]=HUC12_string[i]
  HW2_beta_12[i,2]=beta
} 

# MID STREAMS: loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(MID2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(MID2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(MID2$HU12_ZoneID)) 

MID2_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
MID2_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
MID2_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:32) {
  sub<-subset(MID2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  MID2_beta_4[i,1]=HUC4_string[i]
  MID2_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:128) {
  sub<-subset(MID2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  MID2_beta_8[i,1]=HUC8_string[i]
  MID2_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:452) {
  sub<-subset(MID2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  MID2_beta_12[i,1]=HUC12_string[i]
  MID2_beta_12[i,2]=beta
} 

# RIVERS : loop to run dissimilarity on all HUCs
HUC4_string <-as.character(unique(RIVER2$HU4_ZoneID)) 
HUC8_string <-as.character(unique(RIVER2$HU8_ZoneID)) 
HUC12_string <-as.character(unique(RIVER2$HU12_ZoneID)) 

RIVER2_beta_4 <- data.frame(HU4_ZoneID=NA, beta=NA)
RIVER2_beta_8 <- data.frame(HU8_ZoneID=NA, beta=NA)
RIVER2_beta_12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:16) {
  sub<-subset(RIVER2, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  RIVER2_beta_4[i,1]=HUC4_string[i]
  RIVER2_beta_4[i,2]=beta
} 

#HUC8
for (i in 1:27) {
  sub<-subset(RIVER2, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  RIVER2_beta_8[i,1]=HUC8_string[i]
  RIVER2_beta_8[i,2]=beta
} 

#HUC12
for (i in 1:36) {
  sub<-subset(RIVER2, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  RIVER2_beta_12[i,1]=HUC12_string[i]
  RIVER2_beta_12[i,2]=beta
} 


#################################################
#################################################
#join the beta outputs 
ISO_fish_beta <-left_join(ISO2, ISO_beta_4, by = 'HU4_ZoneID')
HWL_fish_beta <-left_join(HW_L2, HWL_beta_4, by = 'HU4_ZoneID')
DRL_fish_beta <-left_join(DR_ST2, DRL_beta_4, by = c('HU4_ZoneID'))
DRLL_fish_beta <-left_join(DR_STLK2, DRLL_beta_4, by = 'HU4_ZoneID')
HWS_fish_beta <-left_join(HW2, HW2_beta_4, by = 'HU4_ZoneID')
MID_fish_beta <-left_join(MID2, MID2_beta_4, by = c('HU4_ZoneID'))
R_fish_beta <-left_join(RIVER2, RIVER2_beta_4, by = 'HU4_ZoneID')

## compare alpha and beta diversity (Pool et al 2014)
ggplot(data = L_fish_beta, (aes(x = exp_rich, y = beta ))) + geom_point() +
  geom_smooth(method='lm') 
ggplot(data = R_fish_beta, (aes(x = exp_rich, y = beta ))) + geom_point() +
  geom_smooth(method='lm') 
ggplot(data = S_fish_beta, (aes(x = exp_rich, y = beta ))) + geom_point() +
  geom_smooth(method='lm') 
### break out by region! see if there is a difference

## bar graph of the disimilarity values within each HUC to compare distribution (left skewed in the Roden 2018 paper)


## making maps 
#base map 
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
submap <- subset(states, region %in% c("michigan", "iowa", "new hampshire", 'maine', 'wisconsin')) # and within that select michigan 
p<-ggplot(data = submap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

#map of beta diversity at the HUC4 scale 
map_beta<-function(x) { # x = a dataframe   
  p + 
    geom_point(data=x, aes(x=LON_DD, y=LAT_DD, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
    labs(color="beta diversity")
}

map_beta(ISO_fish_beta)
map_beta(HWL_fish_beta)
map_beta(DRL_fish_beta)
map_beta(DRLL_fish_beta)
map_beta(HWS_fish_beta)
map_beta(MID_fish_beta)
map_beta(R_fish_beta)


beta_I<-as.data.frame(dplyr::select(ISO_fish_beta, SITE_ID, beta)) 
beta_I$CLASS<-'ISO'
beta_H<-as.data.frame(dplyr::select(HWL_fish_beta, SITE_ID, beta))
beta_H$CLASS<-'HW_LAKE'
beta_D<-as.data.frame(dplyr::select(DRL_fish_beta, SITE_ID, beta))
beta_D$CLASS<-'DR_ST'
beta_L<-as.data.frame(dplyr::select(DRLL_fish_beta, SITE_ID, beta))
beta_L$CLASS<-'DR_STLK'
beta_W<-as.data.frame(dplyr::select(HWS_fish_beta, SITE_ID, beta))
beta_W$CLASS<-'HW_ST'
beta_M<-as.data.frame(dplyr::select(MID_fish_beta, SITE_ID, beta))
beta_M$CLASS<-'MID'
beta_R<-as.data.frame(dplyr::select(R_fish_beta, SITE_ID, beta))
beta_R$CLASS<-'RIVER'

allbetas<-gtools::smartbind(beta_I, beta_H, beta_D, beta_L,
                  beta_W, beta_M, beta_R) 
allbetas$CLASS<-as.factor(allbetas$CLASS)
allbetas$CLASS <- ordered(allbetas$CLASS, c("HW_LAKE", "DR_ST", "RIVER", "DR_STLK", "ISO", "HW_ST", "MID"))

ggplot(allbetas, aes(x = CLASS, y = beta, color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL)   # Remove x axis label
#save a plot
#cowplot::save_plot("Figures/HU4_lake_beta.pdf", HUC4_lake_beta, base_width = 6,
     #              base_height = 5, dpi=600)


#------ calc Beta by ecosystem type -----
#combine datasets ##
library(data.table)
L_dat<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class) 
S_dat<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, order_class) 
setnames(S_dat, old=c("COMIDv2_JR", "ind_count", 'order_class'), new=c("SITE_ID", "TOTAL_COUNT", "CLASS"))
setnames(L_dat, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))


S_dat$SITE_ID<-as.character(as.integer(S_dat$SITE_ID))
allecos<-gtools::smartbind(L_dat, S_dat)
allecos<-allecos[!(is.na(allecos$CLASS)),]

#group by class and species to add up all the species for a TYPE
d2 <-allecos %>% group_by(CLASS, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
d3<-tidyr::spread(d2, SPP_CODE_new, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate beta diversity between types
   
new <- subset(d3, select = -c(CLASS))

#dissimilarity 1=lakes, 2=res, 3=streams
dis_NL_RES<-vegdist(new, "bray", binary = TRUE) 

####### ####### ####### ####### ####### ####### 
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

#------ alpha diversity by ecosystem type ----- 
lakes<-lake_dat[!duplicated(paste(lake_dat$LAKE_ID)),] 
summary(lakes$conn_class)
dataL<-dplyr::select(lakes, LAKE_ID, LAT_DD, LON_DD, obs_spec, exp_rich, conn_class) 
setnames(dataL, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))
dataL$SITE_ID<-as.character(dataL$SITE_ID)
ISO<-filter(dataL, CLASS == "Isolated")
HW_L<-filter(dataL, CLASS == "Headwater")
DR_ST<-filter(dataL, CLASS== "DR_Stream")
DR_STLK<-filter(dataL, CLASS == "DR_LakeStream")

stream_info<-read.csv("Datasets/stream_reach_info.csv")
dataS<-dplyr::select(stream_info, COMIDv2_JR, lat, lon, obs_spec, exp_rich, order_class) 
setnames(dataS, old=c("COMIDv2_JR", "lat", "lon", "order_class"), new=c("SITE_ID", "LAT_DD", "LON_DD", "CLASS"))
dataS$SITE_ID<-as.character(dataS$SITE_ID)
HW<-filter(dataS, CLASS == "HW")
MID<-filter(dataS, CLASS == "MID")
RIVER<-filter(dataS, CLASS == "RIVER")

allecos<-gtools::smartbind(HW, MID, RIVER,ISO, HW_L, DR_ST, DR_STLK)
summary(allecos$CLASS)

#ANOVA 
anova(lm(exp_rich ~ CLASS, data = allecos))
library(agricolae) #library needed for LSD test 
LSDout<-LSD.test(allecos$exp_rich, allecos$CLASS, 2569, 38.0, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

anova(lm(obs_spec ~ CLASS, data = allecos))
LSDout<-LSD.test(allecos$obs_spec, allecos$CLASS, 2569, 20.0, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

# boxplots to visualize data 
ggplot(allecos, aes(x = reorder(CLASS, exp_rich, FUN=median), y = exp_rich, color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL)   # Remove x axis label

## distribution plots by groups
a <- ggplot(allecos, aes(x = exp_rich))
a+geom_density(aes(fill = CLASS), alpha = 0.4) 

## frequency plots of the classes
ggplot(allecos, aes(CLASS)) +
  geom_bar(fill = "#0073C2FF") 

L_alpha<-p + 
  geom_point(data=L_fish_beta, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")
cowplot::save_plot("Figures/L_alpha.pdf", L_alpha, base_width = 6,
                   base_height = 5, dpi=600)
R_alpha<-p + 
  geom_point(data=R_fish_beta, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")
cowplot::save_plot("Figures/R_alpha.pdf", R_alpha, base_width = 6,
                   base_height = 5, dpi=600)

p + 
  geom_point(data=HW, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("headwater streams")
cowplot::save_plot("Figures/S_alpha.pdf", S_alpha, base_width = 6,
                   base_height = 5, dpi=600)

p + 
  geom_point(data=MID, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("mid-reach streams")

p + 
  geom_point(data=RIVER, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("rivers")

p + 
  geom_point(data=ISO, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("isolated lakes")

p + 
  geom_point(data=HW_L, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("headwater lakes")

p + 
  geom_point(data=DR_ST, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("drainage lakes")

p + 
  geom_point(data=DR_STLK, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity") + 
  ggtitle("drainage lakes with upstream lakes")



### PLOT distribution of stream sizes 
p + geom_point(data=dataS, aes(x=LON_DD, y=LAT_DD, colour = CLASS), alpha=0.7) +
  scale_color_manual(values = c('#66c2a5', '#fc8d62','#8da0cb')) +
  labs(color="stream class")

p+geom_point(data=dataL, aes(x = LON_DD, y = LAT_DD, colour=CLASS), alpha=0.7) + 
  scale_color_manual(values = c('#a6611a', '#dfc27d','#80cdc1','#018571')) +
  labs(color="lake class")


summary(S_fish_beta)