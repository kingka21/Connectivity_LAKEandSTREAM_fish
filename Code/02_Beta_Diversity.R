#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Fall 2018
## Updated: updated Aug 14, 2019 

#load libraries
library(vegan)

#------ calculate beta diversity for HUC4s -------
NL<-filter(lakesall, TYPE == "NL")
RES<-filter(lakesall, TYPE == "RES")

#filter to just columns we need
dataL<-dplyr::select(NL, LAKE_ID, SPP_CODE_new, TOTAL_COUNT) 
dataR<-dplyr::select(RES, LAKE_ID, SPP_CODE_new, TOTAL_COUNT) 
dataS<-dplyr::select(streamsall, COMIDv2_JR, SPP_CODE_new, ind_count) 

#group by eco and species to add up all the species for each ind. eco
dataL2 <-dataL %>% group_by(LAKE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
dataR2 <-dataR %>% group_by(LAKE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
dataS2 <-dataS %>% group_by(COMIDv2_JR, SPP_CODE_new) %>% summarise(total=sum(ind_count))

dataL3<-tidyr::spread(dataL2, SPP_CODE_new, total) #330 lakes
dataR3<-tidyr::spread(dataR2, SPP_CODE_new, total) #263 res
dataS3<-tidyr::spread(dataS2, SPP_CODE_new, total) #1983 streams

dataL3[is.na(dataL3)] <- 0 #NAs to 0 
dataR3[is.na(dataR3)] <- 0 #NAs to 0 
dataS3[is.na(dataS3)] <- 0 #NAs to 0 

L_HUC_codes<-dplyr::select(NL, LAKE_ID, lagoslakeid, HU4_ZoneID, LAT_DD, LON_DD, exp_rich)
R_HUC_codes<-dplyr::select(RES, LAKE_ID, lagoslakeid, HU4_ZoneID, LAT_DD, LON_DD, exp_rich)
S_HUC_codes<-dplyr::select(streamsall, COMIDv2_JR, ZoneID, lat, lon, exp_rich)

Lcodes<-L_HUC_codes[!duplicated(paste(L_HUC_codes$LAKE_ID)),] 
Rcodes<-R_HUC_codes[!duplicated(paste(R_HUC_codes$LAKE_ID)),] 
Scodes<-S_HUC_codes[!duplicated(paste(S_HUC_codes$COMIDv2_JR)),] 

dataL4<-dplyr::left_join(dataL3, Lcodes, by = "LAKE_ID")
dataR4<-dplyr::left_join(dataR3, Rcodes, by = "LAKE_ID")
dataS4<-dplyr::left_join(dataS3, Scodes, by = "COMIDv2_JR")

summary(dataL4$HU4_ZoneID) #how many lake in each HUC 
summary(dataR4$HU4_ZoneID) #how many res in each HUC 
summary(dataS4$ZoneID) #how many stream in each HUC 

### create a function to calculate beta diversity average for a region
calcbeta<-function(x) { # x = a dataframe   
  new<- subset(x, select = -c(LAKE_ID, lagoslakeid, HU4_ZoneID, LAT_DD, LON_DD, exp_rich))
  #dissimilarity
  dis<-vegdist(new, "bray", binary = TRUE) 
  #average dissimilarity
  avg<-mean(dis)
  return(avg)
}


# loop to run dissimilarity on all HUC 4s
LHUC_string <-as.character(unique(dataL4$HU4_ZoneID)) #lakes

L_output_df <- data.frame(HU4_ZoneID=NA, beta=NA)  # lakes          
for (i in 1:28) {
  sub<-subset(dataL4, HU4_ZoneID == LHUC_string[i] )
  beta<-calcbeta(sub)
  L_output_df[i,1]=LHUC_string[i]
  L_output_df[i,2]=beta
} 

RHUC_string <-as.character(unique(dataR4$HU4_ZoneID)) #res

R_output_df <- data.frame(HU4_ZoneID=NA, beta=NA)  # res          
for (i in 1:28) {
  sub<-subset(dataR4, HU4_ZoneID == RHUC_string[i] )
  beta<-calcbeta(sub)
  R_output_df[i,1]=RHUC_string[i]
  R_output_df[i,2]=beta
} 


calcbetaS<-function(x) { # x = a dataframe   
  new<- subset(x, select = -c(COMIDv2_JR, ZoneID, lat, lon, exp_rich))
  #dissimilarity
  dis<-vegdist(new, "bray", binary = TRUE) 
  #average dissimilarity
  avg<-mean(dis)
  return(avg)
}

SHUC_string <-as.character(unique(dataS4$ZoneID)) #stream

S_output_df <- data.frame(HU4_ZoneID=NA, beta=NA)  # stream          
for (i in 1:32) {
  sub<-subset(dataS4, ZoneID == SHUC_string[i] )
  beta<-calcbetaS(sub)
  S_output_df[i,1]=SHUC_string[i]
  S_output_df[i,2]=beta
} 

#join the beta outputs 
L_fish_beta <-left_join(dataL4, L_output_df, by = 'HU4_ZoneID')
R_fish_beta <-left_join(dataR4, R_output_df, by = 'HU4_ZoneID')
S_fish_beta <-left_join(dataS4, S_output_df, by = c('ZoneID' = 'HU4_ZoneID'))

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
HUC4_lake_beta<-p + 
  geom_point(data=L_fish_beta, aes(x=LON_DD, y=LAT_DD, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
  labs(color="beta diversity")
cowplot::save_plot("Figures/HU4_lake_beta.pdf", HUC4_lake_beta, base_width = 6,
                   base_height = 5, dpi=600)

HUC4_res_beta<-p + 
  geom_point(data=R_fish_beta, aes(x=LON_DD, y=LAT_DD, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
  labs(color="beta diversity")
cowplot::save_plot("Figures/HU4_res_beta.pdf", HUC4_res_beta, base_width = 6,
                   base_height = 5, dpi=600)

HUC4_stream_beta<-p + 
  geom_point(data=S_fish_beta, aes(x=lon, y=lat, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
  labs(color="beta diversity")
cowplot::save_plot("Figures/HU4_stream_beta.pdf", HUC4_stream_beta, base_width = 6,
                   base_height = 5, dpi=600)


#map of alpha diversity 
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

S_alpha<-p + 
  geom_point(data=S_fish_beta, aes(x=lon, y=lat, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")
cowplot::save_plot("Figures/S_alpha.pdf", S_alpha, base_width = 6,
                   base_height = 5, dpi=600)

summary(S_fish_beta)
#------ calc Beta by ecosystem type -----
#combine datasets ##
library(data.table)
lake_dat<-dplyr::select(NL, LAKE_ID, SPP_CODE_new, TOTAL_COUNT) 
res_dat<-dplyr::select(RES, LAKE_ID, SPP_CODE_new, TOTAL_COUNT) 
st_dat<-dplyr::select(streamsall, COMIDv2_JR, SPP_CODE_new, ind_count) 
setnames(st_dat, old=c("COMIDv2_JR", "ind_count"), new=c("SITE_ID", "TOTAL_COUNT"))
setnames(lake_dat, old=c("LAKE_ID"), new=c("SITE_ID"))
setnames(res_dat, old=c("LAKE_ID"), new=c("SITE_ID"))
#add a type column 
lake_dat$TYPE<-"LAKE"
res_dat$TYPE<-"RES"
st_dat$TYPE<-"STREAM"

st_dat$SITE_ID<-as.character(as.integer(st_dat$SITE_ID))
allecos<-gtools::smartbind(lake_dat, res_dat, st_dat)

###CHANGED ABOVE SO THIS DOESNT WORK NOW 
#ANOVA 
anova(lm(exp_rich ~ TYPE, data = allecos))
library(agricolae) #library needed for LSD test 
LSDout<-LSD.test(allecos$exp_rich, allecos$TYPE, 2569, 48.2, alpha=0.05)  #specify the DF and MSE of the residuals

###CHANGED ABOVE SO THIS DOESNT WORK NOW 
# boxplots to visualize data 
ggplot(allecos, aes(x = TYPE, y = exp_rich, color = TYPE)) +
  geom_boxplot() +
  scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL)   # Remove x axis label

###CHANGED ABOVE SO THIS DOESNT WORK NOW 
## distribution plots by groups
a <- ggplot(allecos, aes(x = exp_rich))
a+geom_density(aes(fill = TYPE), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

#filter to just columns we need
d1<-dplyr::select(allecos, SITE_ID, SPP_CODE_new, TOTAL_COUNT, TYPE) 

#group by TYPE and species to add up all the species for a TYPE
d2 <-allecos %>% group_by(TYPE, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
d3<-tidyr::spread(d2, SPP_CODE_new, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate beta diversity between types
   
new <- subset(d3, select = -c(TYPE))

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
