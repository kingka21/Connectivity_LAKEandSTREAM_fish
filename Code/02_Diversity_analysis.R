######################################################################################################
#### This code provides methods and some data for the following manuscript: ####

#King, K., Bremigan, M.T., Infante, D., and Cheruvelil, K.S. 2020.
#Surface water connectivity affects lake and stream fish species richness and composition.
#Canadian Journal of Fisheries and Aquatic Sciences 

## Code written by Katelyn King: Fall 2018 - Spring 2020; revised Sep-Nov 2020; manuscript accepted Nov 2020
#Note that the below code reflects code used in the manuscript, however, results in manuscript will differ due to WI data being excluded 
#** Wisconsin has data sharing restrictions for data, these data not included in code 
######################################################################################################

#------ load libraries----- 
library(vegan) #for diversity metrics
library(dplyr) # manipulating data sets
library(ggplot2) #graphing 
library(agricolae) #library needed for LSD test 
library(emmeans) #for ANCOVA 

#------ load and combine diversity data ----- 
#load data
lake_dat<-read.csv('Datasets/lakes_withoutWI.csv')
stream_dat<-read.csv('Datasets/streams_withoutWI.csv')


#select out needed columns for analysis and join lakes and streams 
lakes<-lake_dat[!duplicated(paste(lake_dat$SITE_ID)),] %>%
  dplyr::select( SITE_ID, EXPECTED_RICH, CONN_CLASS, AREA_sqkm, STATE, TYPE) 


streams<-stream_dat[!duplicated(paste(stream_dat$SITE_ID)),] %>%
  dplyr::select(SITE_ID, EXPECTED_RICH, CONN_CLASS, AREA_sqkm, STATE, TYPE) 

#merge datasets
allecos<-gtools::smartbind(lakes, streams)
summary(allecos$CONN_CLASS)

allecos <- allecos %>%
  mutate(REGION = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                          #  STATE == 'WI' ~ "MW", #note published dataset does not include WI
                            STATE =='NH' ~"NE"
  )) 

allecos$REGION<-as.factor(allecos$REGION)

#### ANOVA ####
hist(allecos$EXPECTED_RICH) 
hist(log(allecos$EXPECTED_RICH))

anova(lm(log(EXPECTED_RICH) ~ CONN_CLASS, data = allecos))
LSD_div<-LSD.test(log(allecos$EXPECTED_RICH), allecos$CONN_CLASS, 1405, 0.3447, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals
LSD_div

# boxplots to visualize data 
#allecos$CONN_CLASS<-ordered(allecos$CONN_CLASS, levels=c("ISO_lake", "HW_stream", "HW_stream_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot 
alpha_box_log<-ggplot(allecos, aes(x = CONN_CLASS, y = log(EXPECTED_RICH), color = TYPE)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "b", fontface=2) +
  annotate("text", x = 2, y = 5, label = "a", fontface=2) +
  annotate("text", x = 3, y = 5, label = "a", fontface=2) +
  annotate("text", x = 4, y = 5, label = "b,c", fontface=2) +
  annotate("text", x = 5, y = 5, label = "b", fontface=2) +
  annotate("text", x = 6, y = 5, label = "c,d", fontface=2) +
  annotate("text", x = 7, y = 5, label = "d,e", fontface=2) +
  annotate("text", x = 8, y = 5, label = "e", fontface=2) +
  labs(y = 'log expected richness') + 
  theme( 
    legend.position = "none",
    axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12),
    panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

#cowplot::save_plot("Figures/Fig3.png", alpha_box_log, base_width = 7,
                  # base_height = 4, dpi=600)

#------ ANCOVA accounting for area ----- 
#summary table of area (sqkm)
allecos %>%
  group_by(CONN_CLASS) %>%
  summarise( 
    n=n(),
    min=min(AREA_sqkm, na.rm=TRUE),
    median=median(AREA_sqkm, na.rm=TRUE), 
    mean=mean(AREA_sqkm, na.rm=TRUE),
    max=max(AREA_sqkm, na.rm=TRUE) 
  )

## remove waterbodies that don't have area  
waterbodies_w_area<-allecos[!(is.na(allecos$AREA_sqkm)),]
summary(waterbodies_w_area$CONN_CLASS)

plot(log(EXPECTED_RICH) ~ AREA_sqkm, data=waterbodies_w_area)
abline(lm(log(EXPECTED_RICH) ~ AREA_sqkm, data=waterbodies_w_area), col="red")

#ANCOVA
class_lm<-lm(log(EXPECTED_RICH) ~ AREA_sqkm*CONN_CLASS, data = waterbodies_w_area)
#ANCOVA output 
anova(class_lm) #area, class, and interaction term significant
summary(class_lm) 
## this is looking at the slopes of area in each class 
class_emtrends<-emtrends(class_lm, "CONN_CLASS", var = "AREA_sqkm") 
class_emtrends
pairs(class_emtrends)

#are residuals normal - yes 
plot(density(class_lm$residuals))
qqnorm(class_lm$residuals)
qqline(class_lm$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

#does each group have similar variance (homogeneity of variance) - yes 
plot(class_lm$residuals~class_lm$fitted.values)
lines(lowess(class_lm$fitted.values,class_lm$residuals), col="blue")

#plot non-transformed data, but report stats from transformed 
#waterbodies_w_area$CONN_CLASS<-ordered(waterbodies_w_area$CONN_CLASS, levels=c( "ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))

slope_plot<-ggplot(waterbodies_w_area, aes(AREA_sqkm, EXPECTED_RICH)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = factor(CONN_CLASS)), lwd = 1, alpha = 0.1) +
  scale_color_manual(values = c("#1b9e77", "#e6ab02",  "#7570b3", "#e7298a",  "darkorange", "#666666", "#66a61e", "#a6761d")) +
  theme_bw() 

slope_plot_truncated<-slope_plot +  xlim(0,15) 

Fig7<-slope_plot_truncated+
  theme(legend.position = c(.45, .8), legend.direction='vertical',
        legend.text=element_text(colour='black', size=8), legend.title=element_blank()) +
  labs(y = 'expected richness', 
       x= "area (km2)") +
  annotate("text", x = 3.6, y = 45, label = c("y=7.6 + 3.5x"), colour = "#1b9e77") +
  annotate("text", x = 3.5, y = 43, label = c("y=11 + 1.5x"), colour = "#e6ab02") +
  annotate("text", x = 3.5, y = 41, label = c("y=10 + 1.0x"), colour = "#7570b3") +
  annotate("text", x = 3.6, y = 39, label = c("y=16 + 0.12x"), colour = "#e7298a") +
  annotate("text", x = 3.5, y = 37, label = c("y=6.6 + 69x"), colour = "darkorange") +
  annotate("text", x = 3.6, y = 35, label = c("y=5.4 + 220x"), colour = "#666666") +
  annotate("text", x = 3.5, y = 33, label = c("y=14 + 24x"), colour = "#66a61e") +
  annotate("text", x = 3.6, y = 31, label = c("y=21 + 0.88x"), colour = "#a6761d")


#cowplot::save_plot("Figures/slope_Fig.png", Fig7, base_width = 7,
  #                 base_height = 6, dpi=600)

#------ calc mean dissimilarity by ecosystem type -----
#load data
lake_dat<-read.csv('Datasets/lakes_withoutWI.csv')
stream_dat<-read.csv('Datasets/streams_withoutWI.csv')

#select variables for this analysis and combine datasets ##
L_dat<-dplyr::select(lake_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, TYPE) 
S_dat<-dplyr::select(stream_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, TYPE) 

allecos<-gtools::smartbind(L_dat, S_dat)

#group by class and species to add up all the species for a TYPE
d2 <-allecos %>% group_by(CONN_CLASS, SPP_CODE) %>% summarise(total=sum(TOTAL_COUNT))
d2$CONN_CLASS<-ordered(d2$CONN_CLASS, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))

d3<-tidyr::spread(d2, SPP_CODE, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CONN_CLASS))

#dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 
dissimilarity

### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 64, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER'
                )
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream', 'HW_stream',
                'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream', 'MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER', 'RIVER')
diss_df[,3] = c(NA, 0.26,0.34,0.38,0.46,0.46,0.53,0.48,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.18,0.23, 0.37, 0.37, 0.40,0.38,
                NA,NA,NA,0.14,0.31,0.34,0.30,0.36,
                NA,NA,NA,NA,0.27, 0.31, 0.25, 0.31,
                NA, NA, NA, NA, NA, 0.15, 0.20, 0.39,
                NA,NA,NA,NA,NA,NA, 0.29, 0.42,
                NA,NA,NA,NA,NA,NA, NA, 0.27,
                NA, NA, NA, NA, NA, NA, NA, NA)
               
            
diss_df$TYPE<-factor(diss_df$TYPE, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', "HW_stream_lake", 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))

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

#cowplot::save_plot("Figures/Fig4.png", Fig4, base_width = 7,
  #                 base_height = 5, dpi=600)


#### PERMANOVA (Permutational multivariate analysis of variance) ####
#to check the significant differences among communities between connectivity groups 
fish2<-subset(allecos, select = -c(CONN_CLASS, TYPE)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                          names_from = SPP_CODE, 
                          values_from = TOTAL_COUNT, 
                          values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                          values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(allecos, SITE_ID, CONN_CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CONN_CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

permanova # this says there are sig dif in communities among classes

#------ supplemental analysis for regional differences -----
#*ANOVA for regions ####
#load data
lake_dat<-read.csv('Datasets/lakes_withoutWI.csv')
stream_dat<-read.csv('Datasets/streams_withoutWI.csv')

#select out needed columns for analysis and join lakes and streams 
lakes<-lake_dat[!duplicated(paste(lake_dat$SITE_ID)),] %>%
  dplyr::select( SITE_ID, EXPECTED_RICH, CONN_CLASS, AREA_sqkm, STATE, TYPE) 


streams<-stream_dat[!duplicated(paste(stream_dat$SITE_ID)),] %>%
  dplyr::select(SITE_ID, EXPECTED_RICH, CONN_CLASS, AREA_sqkm, STATE, TYPE) 

#merge datasets
allecos<-gtools::smartbind(lakes, streams)
summary(allecos$CONN_CLASS)

allecos <- allecos %>%
  mutate(REGION = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                            #  STATE == 'WI' ~ "MW", #note published dataset does not include WI
                            STATE =='NH' ~"NE"
  )) 

allecos$REGION<-as.factor(allecos$REGION)

midwest<-filter(allecos, REGION =="MW")
northeast<-filter(allecos, REGION =="NE")

#ANOVA for midwest only 
anova(lm(log(EXPECTED_RICH) ~ CONN_CLASS, data = midwest))
LSD_mw<-LSD.test(log(midwest$EXPECTED_RICH), midwest$CONN_CLASS, 1113, 0.3065, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals
LSD_mw

# boxplots to visualize data 
#midwest$CONN_CLASS<-ordered(midwest$CONN_CLASS, levels=c("ISO_lake", "HW_stream", "HW_stream_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot
box_mw<-ggplot(midwest, aes(x = CONN_CLASS, y = log(EXPECTED_RICH), color = TYPE)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "a,b", fontface=2) +
  annotate("text", x = 2, y = 5, label = "a", fontface=2) +
  annotate("text", x = 3, y = 5, label = "a,b,c", fontface=2) +
  annotate("text", x = 4, y = 5, label = "c,d", fontface=2) +
  annotate("text", x = 5, y = 5, label = "b,c", fontface=2) +
  annotate("text", x = 6, y = 5, label = "d", fontface=2) +
  annotate("text", x = 7, y = 5, label = "d,e", fontface=2) +
  annotate("text", x = 8, y = 5, label = "e", fontface=2) +
  labs(y = 'log expected richness') + 
  theme( 
    legend.position = "none",
    axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=12),
    panel.background = element_rect(colour = "black", size=.5, fill=NA)
  )

#ANOVA for northeast only 
anova(lm(log(EXPECTED_RICH) ~ CONN_CLASS, data = northeast))
LSD_ne<-LSD.test(log(northeast$EXPECTED_RICH), northeast$CONN_CLASS, 284, 0.2304, alpha=0.05, p.adj = "bonferroni")  #specify the DF and MSE of the residuals
LSD_ne

# boxplots to visualize data 
#northeast$CONN_CLASS<-ordered(northeast$CONN_CLASS, levels=c("ISO_lake", "HW_stream", "HW_stream_lake", "HW_lake", "DR_lake", "MID_stream", "DR_lake_lake", "RIVER"))

#log transformed boxplot
box_ne<-ggplot(northeast, aes(x = CONN_CLASS, y = log(EXPECTED_RICH), color = TYPE)) +
  geom_boxplot() +
  scale_color_manual(values = c("purple", "orange" )) + 
  labs(x = NULL) +   # Remove x axis label 
  annotate("text", x = 1, y = 5, label = "a", fontface=2) +
  annotate("text", x = 2, y = 5, label = "a", fontface=2) +
  annotate("text", x = 3, y = 5, label = "a", fontface=2) +
  annotate("text", x = 4, y = 5, label = "a,b", fontface=2) +
  annotate("text", x = 5, y = 5, label = "b", fontface=2) +
  annotate("text", x = 6, y = 5, label = "b", fontface=2) +
  annotate("text", x = 7, y = 5, label = "c", fontface=2) +
  annotate("text", x = 8, y = 5, label = "b,c", fontface=2) +
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

#cowplot::save_plot("Figures/Supp2.png", Supp2, base_width = 7,
  #                 base_height = 4, dpi=600)

#*dissimilarity for regions ####
#load data
lake_dat<-read.csv('Datasets/lakes_withoutWI.csv')
stream_dat<-read.csv('Datasets/streams_withoutWI.csv')

#select variables and combine datasets ##
L_dat<-dplyr::select(lake_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, STATE, TYPE) 
S_dat<-dplyr::select(stream_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, STATE, TYPE) 

allecos<-gtools::smartbind(L_dat, S_dat)

allecos <- allecos %>%
  mutate(REGION = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                          #  STATE == 'WI' ~ "MW", #note WI data not published
                            STATE =='NH' ~"NE"
  )) 

allecos$REGION<-as.factor(allecos$REGION)

midwest<-filter(allecos, REGION =="MW")
northeast<-filter(allecos, REGION =="NE")

#midwest only 
d2 <-midwest %>% group_by(CONN_CLASS, SPP_CODE) %>% summarise(total=sum(TOTAL_COUNT))
d2$CONN_CLASS<-ordered(d2$CONN_CLASS, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))

d3<-tidyr::spread(d2, SPP_CODE, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CONN_CLASS))

#dissimilarity for midwest only 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 
dissimilarity

### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 64, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER'
)
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream', 'HW_stream',
                'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream', 'MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER', 'RIVER')
diss_df[,3] = c(NA, 0.18,0.28,0.31,0.43, 0.44,0.50,0.46,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.19,0.25, 0.40, 0.43, 0.43, 0.39,
                NA,NA,NA,0.15,0.32,0.37,0.32,0.39,
                NA,NA,NA,NA,0.30,0.34, 0.28, 0.31,
                NA, NA, NA, NA, NA, 0.18,0.21, 0.43,
                NA,NA,NA,NA,NA,NA, 0.32, 0.48, 
                NA,NA,NA,NA,NA,NA, NA, 0.31,
                NA, NA, NA, NA, NA, NA, NA, NA)

diss_df$TYPE<-factor(diss_df$TYPE, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', "HW_stream_lake", 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))


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
d2 <-northeast %>% group_by(CONN_CLASS, SPP_CODE) %>% summarise(total=sum(TOTAL_COUNT))
d2$CONN_CLASS<-ordered(d2$CONN_CLASS, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))

d3<-tidyr::spread(d2, SPP_CODE, total) 
d3[is.na(d3)] <- 0 #NAs to 0 

### calculate dissimilarity between types
new <- subset(d3, select = -c(CONN_CLASS))

#**dissimilarity 
dissimilarity<-vegdist(new, "bray", binary = TRUE) #presence/absence when binary is TRUE 
dissimilarity
### create a dissimilarity matrix 
diss_df<-data.frame(matrix(NA, nrow = 64, ncol = 3))
colnames(diss_df) <- c('NAME','TYPE',	'VALUE')
diss_df[,1] = c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER', 
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER',
                'ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake','MID_stream', 'RIVER'
)
diss_df[,2] = c('ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake','ISO_lake',
                'HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake','HW_lake',
                'DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake','DR_lake',
                'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake', 'DR_lake_lake',
                'HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream','HW_stream', 'HW_stream',
                'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake', 'HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake','HW_stream_lake',
                'MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream','MID_stream', 'MID_stream',
                'RIVER','RIVER','RIVER','RIVER','RIVER','RIVER','RIVER', 'RIVER')
diss_df[,3] = c(NA, 0.87,0.90,0.91,0.89,0.87,0.91, 1,    # these numbers are pulled from the "dissimilarity" output
                NA,NA,0.19,0.25, 0.37, 0.27,0.31,0.44,
                NA,NA,NA,0.17,0.26,0.27, 0.26, 0.48,
                NA,NA,NA,NA,0.26, 0.26, 0.18, 0.46,
                NA, NA, NA, NA, NA, 0.12, 0.19, 0.44,
                NA,NA,NA,NA,NA,NA, 0.25, 0.39,
                NA,NA,NA,NA,NA,NA, NA, 0.38,
                NA, NA, NA, NA, NA, NA, NA, NA)


diss_df$TYPE<-factor(diss_df$TYPE, levels=c("ISO_lake", "HW_lake", "DR_lake", "DR_lake_lake", "HW_stream", "HW_stream_lake", "MID_stream", "RIVER"))
diss_df$NAME<-factor(diss_df$NAME, levels=c('RIVER', 'MID_stream', "HW_stream_lake", 'HW_stream', 'DR_lake_lake', 'DR_lake',  'HW_lake', 'ISO_lake' ))

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


#cowplot::save_plot("Figures/Supp2diss.png", Supp2diss, base_width = 10,
  #                 base_height = 5, dpi=600)

#PERMANOVA for MW
fish2<-subset(midwest, select = -c(CONN_CLASS, TYPE, STATE, REGION)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                                 names_from = SPP_CODE, 
                                 values_from = TOTAL_COUNT, 
                                 values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                                 values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(midwest, SITE_ID, CONN_CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CONN_CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

# AOV table 
print(as.data.frame(permanova$aov.tab)) # this says there are sig dif in communities among classes

#PERMANOVA for NE
fish2<-subset(northeast, select = -c(CONN_CLASS, TYPE, STATE, REGION)) 
allnewformat<-tidyr::pivot_wider(data= fish2, 
                                 names_from = SPP_CODE, 
                                 values_from = TOTAL_COUNT, 
                                 values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                                 values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
abund <- subset( allnewformat, select = -SITE_ID )

names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(northeast, SITE_ID, CONN_CLASS)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

permanova <- adonis(abund ~  CONN_CLASS, data=connyids,     #function vegdist will be used to find the dissimilarities. 
                    permutations=999, method = "bray", binary = TRUE) #Perform presence/absence standardization before analysis: (A+B-2*J)/(A+B)

# AOV table 
print(as.data.frame(permanova$aov.tab)) # this says there are sig dif in communities among classes


