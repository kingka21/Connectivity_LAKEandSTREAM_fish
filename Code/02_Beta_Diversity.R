#### Chap 2: Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Fall 2018
## Updated: updated Nov 11, 2019 

#load libraries
library(vegan) #for diversity metrics
library(dplyr) # manipulating data sets
library(ggplot2) #graphing 
library(rgdal) # reading shapefiles 
library(data.table) #re-naming columns
library(agricolae) #library needed for LSD test 
library(tmap) #maping HUC4 with beta values
library(ggsn) #add scale bar and north arrow 

#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/stream_data_QAQC.csv')
stream_info<-read.csv('Datasets/stream_reach_info.csv')

#merge
st_order<-dplyr::select(stream_info, COMIDv2_JR, order_class)
stream_dat<-left_join(st_order, stream_dat)

#use subset of streams and lakes 
st_subset<-dplyr::select(streams_subset, COMIDv2_JR, order_class)
stream_dat<-left_join(st_subset, stream_dat)

lk_subset<-dplyr::select(lakes_subset, lagoslakeid)
lake_dat<-left_join(lk_subset, lake_dat)


#------ calc Beta by ecosystem type -----
#combine datasets ##
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

#dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) 

### creat a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 49, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('DR_lake_lake','DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'DR_lake_lake', 'DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'DR_lake_lake','DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'DR_lake_lake','DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'DR_lake_lake', 'DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'DR_lake_lake', 'DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'DR_lake_lake','DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER'
                )
diss_df[,2] = c('DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER')
diss_df[,3] = c(NA, 0.14,0.23,0.37,0.27,0.26,0.32,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.18,0.33, 0.32,0.30,0.37,
                NA,NA,NA,0.25,0.38,0.41,0.39,
                NA,NA,NA,NA,0.48,0.53, 0.49,
                NA, NA, NA, NA, NA, 0.19, 0.37,
                NA,NA,NA,NA,NA,NA, 0.27,
                NA, NA, NA, NA, NA, NA, NA)
               
            
diss_df$TYPE<-factor(diss_df$TYPE, levels=c('DR_lake_lake','DR_lake', 'HW_lake',	'ISO_lake',	'HW_stream', 'MID_stream', 'RIVER'))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', 'HW_stream', 'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake'))

ggplot(data = diss_df, aes(x = TYPE, y = NAME)) +
  geom_tile(aes(fill = VALUE)) + 
  geom_text(data=diss_df, aes(TYPE, NAME, label = VALUE), color="black", size=5) +
  scale_fill_gradient(name = "dissimilarity",
                      low = "yellow",
                      high = "darkgreen") +
  scale_x_discrete(position = "top") 


#------ alpha diversity by ecosystem type ----- 
#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/stream_data_QAQC.csv')
stream_info<-read.csv('Datasets/stream_reach_info.csv')

#merge
st_ord_area<-dplyr::select(stream_info, COMIDv2_JR, order_class, area)
stream_dat<-left_join(stream_dat, st_ord_area, by = "COMIDv2_JR")

### use subset of streams and lakes 
st_subset<-dplyr::select(streams_subset, COMIDv2_JR)
stream_dat<-left_join(st_subset, stream_dat)

lk_subset<-dplyr::select(lakes_subset, lagoslakeid)
lake_dat<-left_join(lk_subset, lake_dat)

#select out needed columns and re-name columns to join lakes and streams 
lakes<-lake_dat[!duplicated(paste(lake_dat$LAKE_ID)),] 
summary(lakes$conn_class)
dataL<-dplyr::select(lakes, LAKE_ID, LAT_DD, LON_DD, obs_spec, exp_rich, conn_class, AreaSqKm) 
setnames(dataL, old=c("LAKE_ID", "conn_class", "AreaSqKm"), new=c("SITE_ID", "CLASS", "area"))
dataL$SITE_ID<-as.character(dataL$SITE_ID)
dataL$Type<-"lake"

streams<-stream_dat[!duplicated(paste(stream_dat$COMIDv2_JR)),] 
dataS<-dplyr::select(streams, COMIDv2_JR, lat, lon, obs_spec, exp_rich, order_class, area) 
setnames(dataS, old=c("COMIDv2_JR", "lat", "lon", "order_class"), new=c("SITE_ID", "LAT_DD", "LON_DD", "CLASS"))
dataS$SITE_ID<-as.character(dataS$SITE_ID)
dataS$Type<-"stream"

allecos<-gtools::smartbind(dataL, dataS)
summary(allecos$CLASS)
allecos<-allecos[!(is.na(allecos$CLASS)),]

## frequency plots chart after subsampling 
frequency_conny_type<-ggplot(allecos, aes(CLASS)) +
  geom_bar(fill = "#0073C2FF") 
cowplot::save_plot("Figures/frequency_conny.pdf", frequency_conny_type, base_width = 7,
                   base_height = 5, dpi=600)

### maps of distribution of stream and lake sample points  
#base map 
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
submap <- subset(states, region %in% c("michigan", "iowa", "new hampshire", 'maine', 'wisconsin')) # and within that select michigan 
p<-ggplot(data = submap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

stream_map<-p + geom_point(data=dataS, aes(x=LON_DD, y=LAT_DD, colour = CLASS), alpha=0.7) +
  scale_color_manual(values = c('#66c2a5', '#fc8d62','#8da0cb')) +
  labs(color="stream class")
cowplot::save_plot("Figures/streams_conny.pdf", stream_map, base_width = 7,
                   base_height = 5, dpi=600)

lake_map<-p+geom_point(data=dataL, aes(x = LON_DD, y = LAT_DD, colour=CLASS), alpha=0.7) + 
  scale_color_manual(values = c('#a6611a', '#dfc27d','#80cdc1','#018571')) +
  labs(color="lake class")
cowplot::save_plot("Figures/lake_conny.pdf", lake_map, base_width = 7,
                   base_height = 5, dpi=600)


all_sites<-p+geom_point(data=allecos, aes(x = LON_DD, y = LAT_DD, colour=Type), size = 0.7, alpha=0.5) + 
  scale_colour_manual(values = c("#7570b3", "#d95f02" ), name = "Freshwater Type") + 
  north(data = submap, symbol=3, scale=.1, location = "bottomright", 
        anchor = c(x = -65, y = 41)) + 
  ggsn::scalebar(data = submap, dist = 100, dist_unit="km", transform = TRUE, model = "WGS84", st.size = 2, location = "bottomright") +
  theme(legend.position = c(0.65, 0.8), legend.text = element_text(size=9) )

cowplot::save_plot("Figures/all_sites.pdf", all_sites, base_width = 7,
                   base_height = 5, dpi=600)

#ANOVA 
hist(log(allecos$exp_rich))
anova(lm(log(exp_rich) ~ CLASS, data = allecos))
LSDout<-LSD.test(log(allecos$exp_rich), allecos$CLASS, 1415, 0.3472, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

anova(lm(exp_rich ~ CLASS, data = allecos))
LSDout<-LSD.test(allecos$exp_rich, allecos$CLASS, 1415, 46.43, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

# boxplots to visualize data 
allecos$CLASS<-ordered(allecos$CLASS, levels=c("HW", "Isolated", "Headwater", "DR_Stream", "MID", "DR_LakeStream", "RIVER"))

alpha_box<-ggplot(allecos, aes(x = CLASS, y = exp_rich, color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 60, label = "a", fontface=2) +
  annotate("text", x = 2, y = 60, label = "a,b", fontface=2) +
  annotate("text", x = 3, y = 60, label = "b", fontface=2) +
  annotate("text", x = 4, y = 60, label = "b", fontface=2) +
  annotate("text", x = 5, y = 60, label = "c", fontface=2) +
  annotate("text", x = 6, y = 60, label = "c", fontface=2) +
  annotate("text", x = 7, y = 60, label = "d", fontface=2) 

cowplot::save_plot("Figures/alpha_box.pdf", alpha_box, base_width = 7,
                            base_height = 5, dpi=600)

ggplot(allecos, aes(x = CLASS, y = log(area), color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL)   

#------ accounting for area ----- 
#summary table of area (sqkm)
allecos %>%
  group_by(Type) %>%
  summarise( 
    n=n(),
    min=min(area, na.rm=TRUE),
    median=median(area, na.rm=TRUE), 
    mean=mean(area, na.rm=TRUE),
    max=max(area, na.rm=TRUE) 
  )
  
## accounting for area  
allecos$density<-allecos$exp_rich/allecos$area
summary(allecos$CLASS)
waterbodies_w_density<-allecos[!(is.na(allecos$density)),]
summary(waterbodies_w_density$CLASS)

#see where we have area info 
p+geom_point(data=waterbodies_w_density, aes(x = LON_DD, y = LAT_DD, colour=CLASS), alpha=0.7) 
streams_with_area<-filter(waterbodies_w_density, Type=="stream")
p+geom_point(data=streams_with_area, aes(x = LON_DD, y = LAT_DD, colour=CLASS), alpha=0.7) 

## ANOVA with richness density 
anova(lm(log(density) ~ CLASS, data = waterbodies_w_density))
LSDout<-LSD.test(log(waterbodies_w_density$density), waterbodies_w_density$CLASS, 1138, 1.24, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

#waterbodies_w_density$CLASS<-ordered(waterbodies_w_density$CLASS, levels=c("HW", "Isolated", "Headwater", "DR_Stream", "MID", "DR_LakeStream", "RIVER"))

rich_area_box<-ggplot(waterbodies_w_density, aes(x = CLASS, y = log(density), color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL) +
  annotate("text", x = 1, y = 15, label = "a", fontface=2) +
  annotate("text", x = 2, y = 15, label = "b", fontface=2) +
  annotate("text", x = 3, y = 15, label = "b", fontface=2) +
  annotate("text", x = 4, y = 15, label = "b", fontface=2) +
  annotate("text", x = 5, y = 15, label = "c", fontface=2) +
  annotate("text", x = 6, y = 15, label = "d", fontface=2) +
  annotate("text", x = 7, y = 15, label = "e", fontface=2) 

cowplot::save_plot("Figures/rich_area_box.pdf", rich_area_box, base_width = 7,
                   base_height = 5, dpi=600)

#linear regression and accounting for area by regression on the residuals 
simple.fit = lm(exp_rich~area, data=waterbodies_w_density)
summary(simple.fit) #positive significant relationsihp with R2=0.08
multi.fit = lm(exp_rich~area + CLASS, data=waterbodies_w_density)
summary(multi.fit)

#Residuals analysis
resid.reg<-lm(simple.fit$resid~waterbodies_w_density$CLASS)
summary(resid.reg)
anova(lm(simple.fit$resid~waterbodies_w_density$CLASS))
LSDout<-LSD.test(simple.fit$resid, waterbodies_w_density$CLASS, 1138, 42.49, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

#add the residuals to the original dataset 
resid<-simple.fit$resid
waterbodies_w_density$resid<-resid

waterbodies_w_density$CLASS <- ordered(waterbodies_w_density$CLASS, c("HW", "Isolated", "Headwater", "DR_Stream",  "MID", "DR_LakeStream", "RIVER"))

ggplot(waterbodies_w_density, aes(x = CLASS, y = resid, color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL) +
  annotate("text", x = 1, y = 40, label = "a", fontface=2) +
  annotate("text", x = 2, y = 40, label = "b", fontface=2) +
  annotate("text", x = 3, y = 40, label = "b,c", fontface=2) +
  annotate("text", x = 4, y = 40, label = "b", fontface=2) +
  annotate("text", x = 5, y = 40, label = "d", fontface=2) +
  annotate("text", x = 6, y = 40, label = "c,d", fontface=2) +
  annotate("text", x = 7, y = 40, label = "e", fontface=2) 


## distribution plots by groups
a <- ggplot(allecos_sub, aes(x = exp_rich))
a+geom_density(aes(fill = CLASS), alpha = 0.4) 


#------ exploring species composition ----- 
#use the dataset called "dat" - this has both streams and lakes and has grouped by waterbody and species
# need to add HUC4 codes
sp_data<-dplyr::left_join(dat, codes2, by = "SITE_ID")

######## Looking at species by waterbody 177 unique species; from below analysis 23 sp were found in only one site 
sp_waterbody<-sp_data %>% 
  group_by(SPP_CODE_new) %>% 
  summarise(total_waterbody=length(unique(SITE_ID))) #count up unique sites where each species was found

#Looking at species by HUC4 #32 species were found in only one HUC 
sp_perHUC4<-sp_data %>% 
  group_by(SPP_CODE_new) %>% 
  summarise(total_HUC4=length(unique(HU4_ZoneID))) #count up unique HUCs where each species was found

#explore species composition of HUC4 that have high beta
unique_sp<-filter(sp_data, SPP_CODE_new == "BLACK_BUFFALO" | SPP_CODE_new == "BLUE_SUCKER"| SPP_CODE_new == "GILT_DARTER" |
       SPP_CODE_new == "GRAVEL_CHUB"| SPP_CODE_new == "MUMMICHOG"| SPP_CODE_new == "PUGNOSE_MINNOW"| 
       SPP_CODE_new == "ROUND_WHITEFISH"| SPP_CODE_new == "SHOVELNOSE_STURGEON"| SPP_CODE_new == "SILVERBAND_SHINER"|
       SPP_CODE_new == "WEED_SHINER"| SPP_CODE_new == "ARCTIC_CHAR"| SPP_CODE_new == "CREEK_CHUBSUCKER"|
       SPP_CODE_new == "RUDD"| SPP_CODE_new == "SPOTTED_GAR"| SPP_CODE_new == "STARHEAD_TOPMINNOW"| 
       SPP_CODE_new == "STRIPED_SHINER"| SPP_CODE_new == "WHITE_CATFISH")

high_zones <-filter(sp_data, HU4_ZoneID =="HU4_7" | HU4_ZoneID == "HU4_31" )

species_in_zones<-high_zones %>% 
  group_by(SPP_CODE_new, HU4_ZoneID) %>%
  summarise(total=sum(total)) 

# find out how many different species (richness) in each HUC4
diversity<-with(MIdata2, tapply(Species_Code, GEAR, FUN = function(x) length(unique(x))))
diversity<-as.data.frame(diversity)
colnames(diversity) <- c("richness")
diversity$GEAR<-row.names(diversity) 

#------ calculate beta diversity for HUC4s -------
#filter to just columns we need and rename them 
L_dat<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, exp_rich) 
S_dat<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, order_class, HU4_ZoneID, exp_rich) 
setnames(S_dat, old=c("COMIDv2_JR", "ind_count", 'order_class'), new=c("SITE_ID", "TOTAL_COUNT", "CLASS"))
setnames(L_dat, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))

S_dat$SITE_ID<-as.character(as.integer(S_dat$SITE_ID))
allecos<-gtools::smartbind(L_dat, S_dat)
allecos<-allecos[!(is.na(allecos$CLASS)),]

#group by site and species to add up all the species for a HUC
dat <- allecos %>% group_by(SITE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT)) #group by eco and species 
dat2<-tidyr::spread(dat, SPP_CODE_new, total) #reformat the data table 
dat2[is.na(dat2)] <- 0 #NAs to 0 
codes<-dplyr::select(allecos, SITE_ID, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, exp_rich, CLASS) #need to join ID and codes back to table
codes2<-codes[!duplicated(paste(codes$SITE_ID)),]
dat3<-dplyr::left_join(dat2, codes2, by = "SITE_ID")

### create a function to calculate beta diversity average for a region
calcbeta<-function(x) { # x = a dataframe   
  new<- subset(x, select = -c(SITE_ID, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, exp_rich, CLASS))
  #dissimilarity
  dis<-vegdist(new, "bray", binary = TRUE) 
  #average dissimilarity
  avg<-mean(dis)
  return(avg)
}


#loop to run dissimilarity on all HUCs 
HUC4_string <-as.character(unique(dat3$HU4_ZoneID)) 
HUC8_string <-as.character(unique(dat3$HU8_ZoneID)) 
HUC12_string <-as.character(unique(dat3$HU12_ZoneID)) 


beta_HUC4 <- data.frame(HU4_ZoneID=NA, beta=NA)
beta_HUC8 <- data.frame(HU8_ZoneID=NA, beta=NA)
beta_HUC12 <- data.frame(HU12_ZoneID=NA, beta=NA)

#HUC4
for (i in 1:32) {
  sub<-subset(dat3, HU4_ZoneID == HUC4_string[i] )
  beta<-calcbeta(sub)
  beta_HUC4[i,1]=HUC4_string[i]
  beta_HUC4[i,2]=beta
} 

#HUC8
for (i in 1:132) {
  sub<-subset(dat3, HU8_ZoneID == HUC8_string[i] )
  beta<-calcbeta(sub)
  beta_HUC8[i,1]=HUC8_string[i]
  beta_HUC8[i,2]=beta
} 


#HUC12
for (i in 1:479) {
  sub<-subset(dat3, HU12_ZoneID == HUC12_string[i] )
  beta<-calcbeta(sub)
  beta_HUC12[i,1]=HUC12_string[i]
  beta_HUC12[i,2]=beta
} 


#join the beta output
fish_beta <-left_join(dat3, beta_HUC4, by = 'HU4_ZoneID')


beta_I<-as.data.frame(dplyr::select(fish_beta, SITE_ID, beta, exp_rich, CLASS)) 

## compare alpha and beta diversity with different conny classes (Pool et al 2014)
beta_I$CLASS <- ordered(beta_I$CLASS, c("HW", "Isolated", "Headwater", "DR_Stream", "DR_LakeStream", "MID", "RIVER"))

alpha_vs_beta<-ggplot(data = beta_I, (aes(x = exp_rich, y = beta, color=CLASS ))) + geom_point() +
  geom_smooth(method='lm', se=FALSE) + 
  theme(legend.position = c(0.85, 0.25))
cowplot::save_plot("Figures/alpha_vs_beta.pdf", alpha_vs_beta, base_width = 7,
                   base_height = 5, dpi=600)

HUC4_beta<-ggplot(beta_I, aes(x = CLASS, y = beta, color = CLASS)) +
  geom_boxplot() +
  #scale_color_manual(values = c("darkblue", "gold", "darkgreen" )) + 
  labs(x = NULL)   # Remove x axis label
#save a plot
cowplot::save_plot("Figures/HU4_beta_box.pdf", HUC4_beta, base_width = 7,
                   base_height = 5, dpi=600)

anova(lm(beta ~ CLASS, data = beta_I))

LSDout<-LSD.test(beta_I$beta, beta_I$CLASS, 1413, 0.0027945, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

## bar graph of the disimilarity values within each HUC to compare distribution (left skewed in the Roden 2018 paper)

####### map HUC4s ######################
#bring in HU4 polygons for visual 
HU4.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU4", layer = "HU4")
crs(HU4.poly) #shows me the projection of the shapefiles so that I can project the same to the points 

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")

####change to an sf object   
sf_HU4<-sf::st_as_sf(HU4.poly)

#have to have the zone id in there every time for this to work 
HU4_5states<-dplyr::filter(sf_HU4, ZoneID == "HU4_24" | ZoneID == "HU4_31" | ZoneID =="HU4_30"|
                             ZoneID == "HU4_37"|ZoneID =="HU4_40" |ZoneID =="HU4_39" |ZoneID =="HU4_32"| 
                             ZoneID == "HU4_41" | ZoneID == "HU4_61" | ZoneID == "HU4_56" | ZoneID == "HU4_33" | ZoneID =="HU4_57" | ZoneID == "HU4_59" 
                           | ZoneID == "HU4_4" | ZoneID == "HU4_2"  | ZoneID =="HU4_35"
                           | ZoneID =="HU4_63" | ZoneID =="HU4_62"| ZoneID == "HU4_27" | ZoneID =="HU4_55" | ZoneID =="HU4_34" 
                           | ZoneID =="HU4_36"| ZoneID == "HU4_28"| ZoneID == "HU4_29"| ZoneID == "HU4_3" | ZoneID == "HU4_1" | ZoneID == "HU4_7" 
                           | ZoneID =="HU4_60" | ZoneID =="HU4_8" | ZoneID == "HU4_9"| ZoneID ==  "HU4_6"| ZoneID ==  "HU4_58")  
#convert it back to a spatial polygons data frame
HU4_5states.sp<-as(HU4_5states, "Spatial")
crs(HU4_5states.sp)
#plot(HU4_5states.sp)

ggplot(data = HU4_5states.sp) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = factor(ZoneID), color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

##merging beta results with huc4 shapefile
beta_HU4_DF <- merge(HU4_5states.sp, beta_HUC4, by.x="ZoneID", by.y= "HU4_ZoneID", all.x=F)

###mapping beta values to fill huc4 regions on US map using Tmap
summary(beta_HUC4)

p1<-c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "black" )

beta_HU4_map<-tm_shape(beta_HU4_DF )+
  tm_fill("beta", style='fixed', title='Beta Diversity', palette=p1, 
          breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
          textNA = 'NA', colorNA = 'gray') + 
  tm_borders("black") + tm_layout(
    legend.title.size = 2,
    legend.text.size = 1,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1) + 
  tm_layout(legend.outside = TRUE)

save_tmap(tm = beta_HU4_map, filename = 'Figures/HU4_beta_map.png',
          dpi = 300)



# just a white map of HUC  4
just_HU4_map<-tm_shape(HU4_5states.sp) + tm_fill("ZoneID") + 
  tm_layout(legend.outside = TRUE)
save_tmap(tm = just_HU4_map, filename = 'Figures/just_huc_map.png',
          dpi = 300)
#map of beta diversity at the HUC4 scale 
#map_beta<-function(x) { # x = a dataframe   
#  p + 
#    geom_point(data=x, aes(x=LON_DD, y=LAT_DD, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
#    labs(color="beta diversity")
#}

#map_beta(ISO_fish_beta)
#map_beta(HWL_fish_beta)
#map_beta(DRL_fish_beta)
#map_beta(DRLL_fish_beta)
#map_beta(HWS_fish_beta)
#map_beta(MID_fish_beta)
#map_beta(R_fish_beta)


#------ extras ----- 

#alpha maps 
group_a<-filter(allecos_sub, CLASS=="HW" | CLASS == "Isolated")
group_b<-filter(allecos_sub, CLASS=="Headwater" | CLASS == "DR_Stream")
group_c<-filter(allecos_sub, CLASS == "DR_LakeStream")
group_d<-filter(allecos_sub, CLASS=="MID" | CLASS == "RIVER")

p + 
  geom_point(data=group_a, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")
cowplot::save_plot("Figures/L_alpha.pdf", L_alpha, base_width = 6,
                   base_height = 5, dpi=600)
p + 
  geom_point(data=group_b, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")

p + 
  geom_point(data=group_c, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")

p + 
  geom_point(data=group_d, aes(x=LON_DD, y=LAT_DD, colour = c(exp_rich))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")

#create a function to reformat each dataset 
reform<-function(x) { # x = a dataframe   
  dat <- x %>% group_by(SITE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT)) #group by eco and species 
  dat2<-tidyr::spread(dat, SPP_CODE_new, total) #reformat the data table 
  dat2[is.na(dat2)] <- 0 #NAs to 0 
  codes<-dplyr::select(x, SITE_ID, HU4_ZoneID, HU8_ZoneID, HU12_ZoneID, LAT_DD, LON_DD, exp_rich) #need to join ID and codes back to table
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

