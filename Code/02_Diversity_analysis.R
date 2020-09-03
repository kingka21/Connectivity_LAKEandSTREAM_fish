#### Fish biodiversity across lakes and streams at the regional scale
## Written by Katelyn King starting Fall 2018 - Spring 2020

#load libraries
library(vegan) #for diversity metrics
library(dplyr) # manipulating data sets
library(ggplot2) #graphing 
library(rgdal) # reading shapefiles 
library(data.table) #re-naming columns
library(agricolae) #library needed for LSD test 
library(readr) # read and stack csvs
library(emmeans) #for ANCOVA 

#------ load and combine diversity data ----- 
#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/stream_w_area.csv')

lk_subset<-dplyr::select(lakes_subset, LAKE_ID)
lake_dat<-left_join(lk_subset, lake_dat)

## change the conny class names 
lake_dat <- lake_dat %>%
  mutate(conn_class = case_when(conn_class == 'DR_LakeStream' ~ 'DR_lake_lake',   #if ~ then 
                                conn_class == 'DR_Stream' ~ 'DR_lake',
                                conn_class == 'Headwater' ~ 'HW_lake',
                                conn_class == 'Isolated' ~ 'ISO_lake')) 

lake_dat$conn_class<-as.factor(lake_dat$conn_class)

lake_dat <- lake_dat %>%
  mutate(State_Name = case_when(State_Name == 'Iowa' ~ 'IA',   #if ~ then 
                                State_Name == 'Maine' ~ 'ME',
                                State_Name == 'Michigan' ~ 'MI',
                                State_Name == 'Minnesota' ~ 'WI',
                                State_Name == 'New Hampshire' ~ 'NH',
                                State_Name == 'OUT_OF_COUNTY_STATE' ~ 'IA',
                                State_Name == 'Wisconsin' ~ 'WI'
                          
  )) 
lake_dat$State_Name<-as.factor(lake_dat$State_Name)

#select out needed columns and re-name columns to join lakes and streams 
lakes<-lake_dat[!duplicated(paste(lake_dat$LAKE_ID)),] 
summary(lakes$conn_class)
dataL<-dplyr::select(lakes, LAKE_ID, LAT_DD, LON_DD, obs_spec, exp_rich, conn_class, AreaSqKm, State_Name) 
data.table::setnames(dataL, old=c("LAKE_ID", "conn_class", "AreaSqKm", 'State_Name'), new=c("SITE_ID", "CLASS", "area", "STATE"))
dataL$SITE_ID<-as.character(dataL$SITE_ID)
dataL$Type<-"lake"

streams<-stream_dat[!duplicated(paste(stream_dat$COMIDv2_JR)),] 
dataS<-dplyr::select(streams, COMIDv2_JR, LAT_DD, LON_DD, obs_spec, exp_rich, CLASS, area, STATE) 
data.table::setnames(dataS, old=c("COMIDv2_JR"), new=c("SITE_ID"))
dataS$SITE_ID<-as.character(dataS$SITE_ID)
dataS$Type<-"stream"

allecos<-gtools::smartbind(dataL, dataS)
summary(allecos$CLASS)

allecos <- allecos %>%
  mutate(Region = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                            STATE == 'WI' ~ "MW",
                            STATE =='NH' ~"NE"
  )) 

allecos$Region<-as.factor(allecos$Region)

#write.csv(allecos, '/Users/katelynking/Desktop/allecos.csv',row.names = FALSE)

#### maps of distribution of stream and lake sample points  ####
#base map 
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
submap <- subset(states, region %in% c("michigan", "iowa", "new hampshire", 'maine', 'wisconsin')) # and within that select michigan 
p<-ggplot(data = submap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

all_sites<-p+geom_point(data=allecos, aes(x = LON_DD, y = LAT_DD, colour=Type), size = 0.8, alpha=0.5) + 
  scale_colour_manual(values = c("#7570b3", "#d95f02" ), name = "freshwater type") + 
  #north(data = submap, symbol=3, scale=.1, location = "bottomright", 
   #     anchor = c(x = -80, y = 41)) + 
  #ggsn::scalebar(data = submap, dist = 200, dist_unit="km", transform = TRUE, model = "WGS84", st.size = 3, st.dist = 0.05, location = "bottomright", 
   #              anchor = c(x = -80, y = 40)) +
  theme(legend.position = c(0.65, 0.5),   
        legend.text = element_text(size=6),  
          legend.title = element_text(size=7),  
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))

stream<-filter(allecos, Type == "stream")
stream_map<-p + geom_point(data=stream, aes(x=LON_DD, y=LAT_DD, colour = CLASS), size = 0.8, alpha=0.7) +
  scale_color_manual(values = c('#66c2a5', '#fc8d62','#8da0cb'), name = "stream class") +
  #north(data = submap, symbol=3, scale=.1, location = "bottomright", 
   #     anchor = c(x = -80, y = 41)) + 
  #ggsn::scalebar(data = submap, dist = 200, dist_unit="km", transform = TRUE, model = "WGS84", st.size = 3, st.dist = 0.05, location = "bottomright", 
     #          anchor = c(x = -80, y = 40)) +
  theme(legend.position = c(0.65, 0.5), 
        legend.text = element_text(size=6),
        legend.title = element_text(size=7), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) 


lake<-filter(allecos, Type == "lake")
lake_map<-p+geom_point(data=lake, aes(x = LON_DD, y = LAT_DD, colour=CLASS), size = 0.8, alpha=0.7) + 
  scale_color_manual(values = c('#a6611a', '#dfc27d','#80cdc1','#018571'), name="lake class") +
  #north(data = submap, symbol=3, scale=.1, location = "bottomright", 
   #     anchor = c(x = -80, y = 41)) + 
  #ggsn::scalebar(data = submap, dist = 100, dist_unit="km", transform = TRUE, model = "WGS84", st.size = 2, location = "bottomright", 
    #             anchor = c(x = -80, y = 40)) +
  theme(legend.position = c(0.65, 0.5), 
        legend.text = element_text(size=6), 
        legend.title = element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) 



panel_3<-cowplot::plot_grid(all_sites, lake_map, stream_map, ncol = 1, nrow = 3)
cowplot::save_plot("Figures/all_sites.png", panel_3, base_width = 7,
                   base_height = 5, dpi=600)

#### ANOVA ####
hist(allecos$exp_rich) 
hist(log(allecos$exp_rich))

anova(lm(log(exp_rich) ~ CLASS, data = allecos))
LSD_div<-LSD.test(log(allecos$exp_rich), allecos$CLASS, 1406, 0.3461, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

# boxplots to visualize data 
allecos$CLASS<-ordered(allecos$CLASS, levels=c("HW_stream", "ISO_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot
alpha_box_log<-ggplot(allecos, aes(x = CLASS, y = log(exp_rich), color = Type)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "a", fontface=2) +
  annotate("text", x = 2, y = 5, label = "b", fontface=2) +
  annotate("text", x = 3, y = 5, label = "b,c", fontface=2) +
  annotate("text", x = 4, y = 5, label = "b", fontface=2) +
  annotate("text", x = 5, y = 5, label = "c,d", fontface=2) +
  annotate("text", x = 6, y = 5, label = "d,e", fontface=2) +
  annotate("text", x = 7, y = 5, label = "e", fontface=2) +
  labs(y = 'log expected richness') + 
  theme( 
    legend.position = "none",
    axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12),
    panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )


#------ accounting for area ----- 
#summary table of area (sqkm)
allecos %>%
  group_by(CLASS) %>%
  summarise( 
    n=n(),
    min=min(area, na.rm=TRUE),
    median=median(area, na.rm=TRUE), 
    mean=mean(area, na.rm=TRUE),
    max=max(area, na.rm=TRUE) 
  )

## remove waterbodies that don't have area  
waterbodies_w_area<-allecos[!(is.na(allecos$area)),]
summary(waterbodies_w_area$CLASS)

plot(log(exp_rich) ~ area, data=waterbodies_w_area)
abline(lm(log(exp_rich) ~ area, data=waterbodies_w_area), col="red")

#ANCOVA
class_lm<-lm(log(exp_rich) ~ area*CLASS, data = waterbodies_w_area)
#ANCOVA output 
anova(class_lm) #area, class, and interaction term significant
summary(class_lm) #HW_lake, area*HW_lake, area*river

#are residuals normal - yes 
plot(density(class_lm$residuals))
qqnorm(class_lm$residuals)
qqline(class_lm$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

#does each group have similar variance (homogeneity of variance) - yes 
plot(class_lm$residuals~class_lm$fitted.values)
lines(lowess(class_lm$fitted.values,class_lm$residuals), col="blue")

#graph to visualize slopes of each class 
waterbodies_w_area$CLASS<-ordered(waterbodies_w_area$CLASS, levels=c("HW_stream", "ISO_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

ggplot(waterbodies_w_area, aes(area, exp_rich)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = factor(STATE)), lwd = 1, alpha = 0.1) +
  scale_color_manual(values = c("tan1", "lightgreen", "lightblue2", "salmon", "plum", "green4", "purple4")) +
  theme_bw() +
  theme(legend.position = c(.9, .69), legend.direction='vertical',
        legend.text=element_text(colour='black', size=8), legend.title=element_blank()) +
  ggpubr:: stat_regline_equation(label.x.npc = .5, label.y.npc = 1,
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = STATE)
  ) + 
  labs(y = 'expected richness', 
       x= "area (km2)") 

#linear regression and accounting for area by regression on the residuals 
#simple.fit = lm(log(exp_rich)~area, data=waterbodies_w_density)
#summary(simple.fit) #positive significant relationsihp with R2=0.08

#Residuals analysis
#resid.reg<-lm(simple.fit$resid~waterbodies_w_density$CLASS)
#summary(resid.reg)
#hist(simple.fit$resid)

#anova(lm(simple.fit$resid~waterbodies_w_density$CLASS))
#LSD_resid<-LSD.test(simple.fit$resid, waterbodies_w_density$CLASS, 1204, 0.3119, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals

#add the residuals to the original dataset for plotting 
waterbodies_w_area$resid<-class_lm$residuals

waterbodies_w_area$CLASS <- ordered(waterbodies_w_area$CLASS, c("HW_stream", "ISO_lake", "HW_lake", "DR_lake",  "MID_stream", "DR_lake_lake", "RIVER"))

resid_box<-ggplot(waterbodies_w_area, aes(x = CLASS, y = resid, color = Type)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange"  )) + 
  labs(x = NULL) +
  annotate("text", x = 1, y = 4, label = "a", fontface=2) +
  annotate("text", x = 2, y = 4, label = "b", fontface=2) +
  annotate("text", x = 3, y = 4, label = "b,c", fontface=2) +
  annotate("text", x = 4, y = 4, label = "b", fontface=2) +
  annotate("text", x = 5, y = 4, label = "c", fontface=2) +
  annotate("text", x = 6, y = 4, label = "c", fontface=2) +
  annotate("text", x = 7, y = 4, label = "d", fontface=2) +
  labs(y = 'residuals') +
  theme(legend.position = "none", 
        axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

Fig3<-cowplot::plot_grid(alpha_box_log, resid_box,
  align = 'vh',
  labels = c("A", "B")
)

cowplot::save_plot("Figures/Fig3.png", Fig3, base_width = 7,
                   base_height = 4, dpi=600)

# compare area of a stream reach to WS area 
elev <- list.files(path = "/Users/katelynking/Desktop/Chap 3 Conny/StreamCAT_data/Elevation", pattern = "*.csv", full.names = T)
ws_tbl <- sapply(elev, read_csv, simplify=FALSE) %>% 
  bind_rows() %>%
  dplyr::select(COMID, WsAreaSqKm) %>% #Definition: Mean of all elevation values(meters) within the total upstream watershed (Ws)
  rename(Area_WS_SqKm = WsAreaSqKm)

dataS$COMID<-as.numeric(dataS$SITE_ID)
area_ws_relation<-left_join(dataS, ws_tbl)
cor(area_ws_relation$area, area_ws_relation$Area_WS_SqKm, use = "complete.obs")
cor.test(area_ws_relation$area, area_ws_relation$Area_WS_SqKm)
plot(area_ws_relation$area, area_ws_relation$Area_WS_SqKm)

#------ calc mean dissimilarity by ecosystem type -----
#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/streams_subset.csv')

lk_subset<-dplyr::select(lakes_subset, LAKE_ID)
lake_dat<-left_join(lk_subset, lake_dat)

lake_dat <- lake_dat %>%
  mutate(conn_class = case_when(conn_class == 'DR_LakeStream' ~ 'DR_lake_lake',   #if ~ then 
                                conn_class == 'DR_Stream' ~ 'DR_lake',
                                conn_class == 'Headwater' ~ 'HW_lake',
                                conn_class == 'Isolated' ~ 'ISO_lake')) 

lake_dat$conn_class<-as.factor(lake_dat$conn_class)

#combine datasets ##
L_dat<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class) 
S_dat<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, CLASS) 
setnames(S_dat, old=c("COMIDv2_JR", "ind_count"), new=c("SITE_ID", "TOTAL_COUNT"))
setnames(L_dat, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))
L_dat$Type<-"lake"
S_dat$Type<-"stream"

S_dat$SITE_ID<-as.character(as.integer(S_dat$SITE_ID))
allecos<-gtools::smartbind(L_dat, S_dat)


#group by class and species to add up all the species for a TYPE
d2 <-allecos %>% group_by(CLASS, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
d3<-tidyr::spread(d2, SPP_CODE_new, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CLASS))

#dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 

### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 49, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'
                )
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER')
diss_df[,3] = c(NA, 0.26,0.34,0.38,0.47,0.53,0.48,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.18,0.23, 0.37,0.40,0.38,
                NA,NA,NA,0.14,0.31,0.30,0.36,
                NA,NA,NA,NA,0.27,0.25, 0.31,
                NA, NA, NA, NA, NA, 0.19, 0.37,
                NA,NA,NA,NA,NA,NA, 0.27,
                NA, NA, NA, NA, NA, NA, NA)
               
            
diss_df$TYPE<-factor(diss_df$TYPE, levels=c('ISO_lake', 'HW_lake', 'DR_lake','DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))

Fig4<-ggplot(data = diss_df, aes(x = TYPE, y = NAME)) +
  geom_tile(aes(fill = VALUE)) + 
  geom_text(data=diss_df, aes(TYPE, NAME, label = VALUE), color="black", size=4) +
  scale_fill_gradient(name = "dissimilarity",
                      low = "yellow",
                      high = "darkgreen") +
  scale_x_discrete(position = "top") +
  labs(x=NULL, y=NULL) + 
  theme( legend.position = c(0.9, 0.7),
        axis.text.x = element_text(size=10,angle = 45, hjust = 0), 
        axis.text.y = element_text(size=10),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

cowplot::save_plot("Figures/Fig4.png", Fig4, base_width = 7,
                   base_height = 5, dpi=600)


#### PERMANOVA (Permutational multivariate analysis of variance) ####
#to check the significant differences among communities between connectivity groups 
fish2<-subset(allecos, select = -c(CLASS, Type)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                          names_from = SPP_CODE_new, 
                          values_from = TOTAL_COUNT, 
                          values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                          values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(allecos, SITE_ID, CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

# AOV table 
print(as.data.frame(permanova$aov.tab)) # this says there are sig dif in communities among classes

#------ supplemental analysis for regional differences -----
midwest<-filter(allecos, Region =="MW")
northeast<-filter(allecos, Region =="NE")

#ANOVA for midwest only 
anova(lm(log(exp_rich) ~ CLASS, data = midwest))
LSD_mw<-LSD.test(log(midwest$exp_rich), midwest$CLASS, 1114, 0.3071, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals
LSD_mw

# boxplots to visualize data 
midwest$CLASS<-ordered(midwest$CLASS, levels=c("HW_stream", "ISO_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot
box_mw<-ggplot(midwest, aes(x = CLASS, y = log(exp_rich), color = Type)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "a", fontface=2) +
  annotate("text", x = 2, y = 5, label = "a", fontface=2) +
  annotate("text", x = 3, y = 5, label = "b,c", fontface=2) +
  annotate("text", x = 4, y = 5, label = "b", fontface=2) +
  annotate("text", x = 5, y = 5, label = "c", fontface=2) +
  annotate("text", x = 6, y = 5, label = "c,d", fontface=2) +
  annotate("text", x = 7, y = 5, label = "d", fontface=2) +
  labs(y = 'log expected richness') + 
  theme( 
    legend.position = "none",
    axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12),
    panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

#ANOVA for northeast only 
anova(lm(log(exp_rich) ~ CLASS, data = northeast))
LSD_ne<-LSD.test(log(northeast$exp_rich), northeast$CLASS, 285, 0.2296, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals
LSD_ne

# boxplots to visualize data 
northeast$CLASS<-ordered(northeast$CLASS, levels=c("HW_stream", "ISO_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot
box_ne<-ggplot(northeast, aes(x = CLASS, y = log(exp_rich), color = Type)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "a", fontface=2) +
  annotate("text", x = 2, y = 5, label = "a", fontface=2) +
  annotate("text", x = 3, y = 5, label = "a,b", fontface=2) +
  annotate("text", x = 4, y = 5, label = "b", fontface=2) +
  annotate("text", x = 5, y = 5, label = "b", fontface=2) +
  annotate("text", x = 6, y = 5, label = "c", fontface=2) +
  annotate("text", x = 7, y = 5, label = "b,c", fontface=2) +
  labs(y = 'log expected richness') + 
  theme( 
    legend.position = "none",
    axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12),
    panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

Supp2<-cowplot::plot_grid(box_mw, box_ne,
                         align = 'vh',
                         labels = c("A", "B")
)

cowplot::save_plot("Figures/Supp2.png", Supp2, base_width = 7,
                   base_height = 4, dpi=600)

#**dissimilarity
#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/streams_subset.csv')

lk_subset<-dplyr::select(lakes_subset, LAKE_ID)
lake_dat<-left_join(lk_subset, lake_dat)

lake_dat <- lake_dat %>%
  mutate(conn_class = case_when(conn_class == 'DR_LakeStream' ~ 'DR_lake_lake',   #if ~ then 
                                conn_class == 'DR_Stream' ~ 'DR_lake',
                                conn_class == 'Headwater' ~ 'HW_lake',
                                conn_class == 'Isolated' ~ 'ISO_lake')) 

lake_dat$conn_class<-as.factor(lake_dat$conn_class)

lake_dat <- lake_dat %>%
  mutate(STATE = case_when(State_Name == 'Iowa' ~ 'IA',   #if ~ then 
                                State_Name == 'Maine' ~ 'ME',
                                State_Name == 'Michigan' ~ 'MI',
                                State_Name == 'Minnesota' ~ 'WI',
                                State_Name == 'New Hampshire' ~ 'NH',
                                State_Name == 'OUT_OF_COUNTY_STATE' ~ 'IA',
                                State_Name == 'Wisconsin' ~ 'WI'
                                
  )) 
lake_dat$STATE<-as.factor(lake_dat$STATE)

#combine datasets ##
L_dat<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class, STATE) 
S_dat<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, CLASS, STATE) 
setnames(S_dat, old=c("COMIDv2_JR", "ind_count"), new=c("SITE_ID", "TOTAL_COUNT"))
setnames(L_dat, old=c("LAKE_ID", "conn_class"), new=c("SITE_ID", "CLASS"))
L_dat$Type<-"lake"
S_dat$Type<-"stream"

S_dat$SITE_ID<-as.character(as.integer(S_dat$SITE_ID))
allecos<-gtools::smartbind(L_dat, S_dat)

allecos <- allecos %>%
  mutate(Region = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                            STATE == 'WI' ~ "MW",
                            STATE =='NH' ~"NE"
  )) 

allecos$Region<-as.factor(allecos$Region)

midwest<-filter(allecos, Region =="MW")
northeast<-filter(allecos, Region =="NE")

#midwest only 
d2 <-midwest %>% group_by(CLASS, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
d3<-tidyr::spread(d2, SPP_CODE_new, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CLASS))

#**dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 

### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 49, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'
)
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER')
diss_df[,3] = c(NA, 0.18,0.28,0.31,0.45,0.50,0.46,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.19,0.25, 0.40,0.43,0.39,
                NA,NA,NA,0.15,0.33,0.32,0.39,
                NA,NA,NA,NA,0.29,0.28, 0.31,
                NA, NA, NA, NA, NA, 0.19, 0.41,
                NA,NA,NA,NA,NA,NA, 0.31,
                NA, NA, NA, NA, NA, NA, NA)


diss_df$TYPE<-factor(diss_df$TYPE, levels=c('ISO_lake', 'HW_lake', 'DR_lake','DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))

Supp2a<-ggplot(data = diss_df, aes(x = TYPE, y = NAME)) +
  geom_tile(aes(fill = VALUE)) + 
  geom_text(data=diss_df, aes(TYPE, NAME, label = VALUE), color="black", size=4) +
  scale_fill_gradient(name = "dissimilarity",
                      low = "yellow",
                      high = "darkgreen") +
  scale_x_discrete(position = "top") +
  labs(x=NULL, y=NULL) + 
  theme( legend.position = c(0.8, 0.7),
         axis.text.x = element_text(size=10,angle = 45, hjust = 0), 
         axis.text.y = element_text(size=10),
         panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )


#northeast only 
d2 <-northeast %>% group_by(CLASS, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))
d3<-tidyr::spread(d2, SPP_CODE_new, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CLASS))

#**dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 

### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 49, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'
)
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER')
diss_df[,3] = c(NA, 0.87,0.90,0.91,0.89,0.91, 1,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.19,0.25, 0.34,0.31,0.44,
                NA,NA,NA,0.17,0.25,0.26,0.48,
                NA,NA,NA,NA,0.24,0.18, 0.46,
                NA, NA, NA, NA, NA, 0.17, 0.41,
                NA,NA,NA,NA,NA,NA, 0.38,
                NA, NA, NA, NA, NA, NA, NA)


diss_df$TYPE<-factor(diss_df$TYPE, levels=c('ISO_lake', 'HW_lake', 'DR_lake','DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))

Supp2b<-ggplot(data = diss_df, aes(x = TYPE, y = NAME)) +
  geom_tile(aes(fill = VALUE)) + 
  geom_text(data=diss_df, aes(TYPE, NAME, label = VALUE), color="black", size=4) +
  scale_fill_gradient(name = "dissimilarity",
                      low = "yellow",
                      high = "darkgreen") +
  scale_x_discrete(position = "top") +
  labs(x=NULL, y=NULL) + 
  theme( legend.position = c(0.8, 0.7),
         axis.text.x = element_text(size=10,angle = 45, hjust = 0), 
         axis.text.y = element_text(size=10),
         panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )


Supp2diss<-cowplot::plot_grid(Supp2a, Supp2b,
                          align = 'vh',
                          labels = c("A", "B")
)


cowplot::save_plot("Figures/Supp2diss.png", Supp2diss, base_width = 10,
                   base_height = 5, dpi=600)

#ANCOVA for MW
fish2<-subset(midwest, select = -c(CLASS, Type, STATE, Region)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                                 names_from = SPP_CODE_new, 
                                 values_from = TOTAL_COUNT, 
                                 values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                                 values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(midwest, SITE_ID, CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

# AOV table 
print(as.data.frame(permanova$aov.tab)) # this says there are sig dif in communities among classes

#ANCOVA for NE
fish2<-subset(northeast, select = -c(CLASS, Type, STATE, Region)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                                 names_from = SPP_CODE_new, 
                                 values_from = TOTAL_COUNT, 
                                 values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                                 values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(northeast, SITE_ID, CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

# AOV table 
print(as.data.frame(permanova$aov.tab)) # this says there are sig dif in communities among classes
