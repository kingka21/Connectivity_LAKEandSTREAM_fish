######################################################################################################
#### This code provides methods and some data for the following manuscript: ####

#King, K., Bremigan, M.T., Infante, D., and Cheruvelil, K.S. 2020.
#Surface water connectivity affects lake and stream fish species richness and composition.
#Canadian Journal of Fisheries and Aquatic Sciences 

## Code written by Katelyn King: Fall 2018 - Spring 2020; revised Sep-Nov 2020; manuscript accepted Nov 2020
#Note that the below code reflects code used in the manuscript, however, results in manuscript will differ due to WI data being excluded 
#** Wisconsin has data sharing restrictions for data, these data not included in code 
######################################################################################################

##------ load libraries----- 
library(tidyr)
library(FactoMineR)
library(factoextra)

##------ load and combine data ----- 
lake_dat<-read.csv('Datasets/lakes_withoutWI.csv')
stream_dat<-read.csv('Datasets/streams_withoutWI.csv')

#select columns needed for analysis 
L_dat<-dplyr::select(lake_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, STATE, TYPE) 
S_dat<-dplyr::select(stream_dat, SITE_ID, SPP_CODE, TOTAL_COUNT, CONN_CLASS, STATE, TYPE) 

#### remove species with < 5% occurrence rates ####
lakes2<-subset(L_dat, select = -c(CONN_CLASS, TYPE, STATE)) 
streams2<-subset(S_dat, select = -c(CONN_CLASS, TYPE, STATE)) 

lakenewformat<-pivot_wider(data= lakes2, #559
                           names_from = SPP_CODE, 
                           values_from = TOTAL_COUNT, 
                           values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                           values_fn = list(TOTAL_COUNT = sum)) 

lakenewformat<-subset(lakenewformat, select = -c(SITE_ID))
lakenewformat[lakenewformat > 0] <- 1 
L_col.sums<-stack(colSums(lakenewformat)/559*100) #sum the number of occurrences and divide by total number of lakes sampled
common_lake<-filter(L_col.sums, values > 5)  #38 species with 5% 
L_subsp<-dplyr::left_join(common_lake, L_dat, by = c("ind" = "SPP_CODE"))
names(L_subsp)[names(L_subsp) == "ind"] <- "SPP_CODE"


streamnewformat<-pivot_wider(data= streams2, #854
                             names_from = SPP_CODE, 
                             values_from = TOTAL_COUNT, 
                             values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                             values_fn = list(TOTAL_COUNT = sum)) 
streamnewformat<-subset(streamnewformat, select = -c(SITE_ID))
streamnewformat[streamnewformat > 0] <- 1 

S_col.sums<-stack(colSums(streamnewformat)/854*100) #sum the number of occurrences and divide by total number of lakes sampled
common_stream<-filter(S_col.sums, values > 5)  #48 species with 5% 
S_subsp<-dplyr::left_join(common_stream, S_dat, by = c("ind" = "SPP_CODE"))
names(S_subsp)[names(S_subsp) == "ind"] <- "SPP_CODE"


alleco_subsp<-gtools::smartbind(L_subsp, S_subsp) 

alleco_subsp <- alleco_subsp %>%
  mutate(REGION = case_when(STATE == 'IA' ~ "MW",   #if ~ then 
                            STATE =='ME' ~ "NE",
                            STATE == 'MI' ~ "MW",
                          #  STATE == 'WI' ~ "MW", #WI data was not published
                            STATE =='NH' ~"NE"
  )) 
alleco_subsp$REGION<-as.factor(alleco_subsp$REGION)

##### look at distinct species across lakes and streams ####
distinct(alleco_subsp, SPP_CODE) #62 common sp

lake_sp<-L_dat %>%
  group_by(SPP_CODE) %>%
  summarise(n=n())

stream_sp<-S_dat %>%
  group_by(SPP_CODE) %>%
  summarise(n=n())

just_lakes<-anti_join(lake_sp, stream_sp, by = "SPP_CODE")
just_streams<-anti_join(stream_sp, lake_sp, by = "SPP_CODE")

#### Correspondence Analysis (CA) ####

# organize data presence absence 
alleco_subsp$CONN_CLASS<-ordered(alleco_subsp$CONN_CLASS, levels=c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'HW_stream_lake', 'MID_stream', 'RIVER'))

fish2<-subset(alleco_subsp, select = -c(CONN_CLASS, TYPE, values, STATE, REGION)) 
allnewformat<-pivot_wider(data= fish2, 
                          names_from = SPP_CODE, 
                          values_from = TOTAL_COUNT, 
                          values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                          values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
presabs <- subset( allnewformat, select = -SITE_ID )
presabs[presabs > 0] <- 1  
presabs<-as.data.frame(ifelse(presabs == 0, "No", "Yes"))

# need conny ids 
names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(alleco_subsp, SITE_ID, CONN_CLASS, STATE, REGION)
conny<-connyclass [!duplicated(paste(connyclass$SITE_ID)),] #
connyids<-left_join(names, conny) #want them in the same order as the sites 

#run MCA 
res.mca <- MCA(presabs, graph = FALSE)

#percentage of explained variance screeplot 
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)) #show screeplot

# results for column categories (sp)
var <- get_mca_var(res.mca)
#The contribution of the variable categories (in %) to the definition of the dimensions can be extracted as follow:
head(round(var$contrib,2), 4)
# Contributions of rows to dimension 1; this reference line corresponds to the expected value if the contribution where uniform.
fviz_contrib(res.mca, choice = "var", axes = 1, top = 5)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 5)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 5)

#add in ellipse base on conny type 
Fig5a<-fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = connyids$CONN_CLASS, # color by groups 
             addEllipses = TRUE,
             palette = c("#1b9e77", "#e6ab02",  "#7570b3", "#e7298a",  "darkorange", "#666666", "#66a61e", "#a6761d"),
             ggtheme = theme_minimal(), 
             title = '') + 
  theme(legend.position = c(.9, .85),
        legend.title = element_blank())


#add in ellipse base on region (northeast vs midwest)
Fig5b<-fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = connyids$REGION, # color by groups 
             addEllipses = TRUE, 
             ggtheme = theme_minimal(), 
             title = '') + 
  theme(legend.position = c(.9, .85),
        legend.title = element_blank())

Fig5<-cowplot::plot_grid(Fig5a, Fig5b,
                         align = 'vh',
                         labels = c("A", "B")
)

#cowplot::save_plot("Figures/Fig5.png", Fig5, base_width = 10,
  #                 base_height = 6, dpi=600)


#### occurrence rates by class for the supp table ####
DR_LL<-filter(L_dat, CONN_CLASS== "DR_lake_lake")
DR_L<-filter(L_dat, CONN_CLASS== "DR_lake")
HW_L<-filter(L_dat, CONN_CLASS== "HW_lake")
ISO_L<-filter(L_dat, CONN_CLASS== "ISO_lake")
HW_S<-filter(S_dat, CONN_CLASS== "HW_stream")
HW_SL<-filter(S_dat, CONN_CLASS== "HW_stream_lake")
MID_S<-filter(S_dat, CONN_CLASS== "MID_stream")
RIVER<-filter(S_dat, CONN_CLASS== "RIVER")

newformat<-HW_SL %>% 
  distinct(SITE_ID, SPP_CODE) %>% 
  count(SPP_CODE)

#for each conny type, number of sites a species is in, divided by total number of sites * 100
newformat$riv_occ<-round(newformat$n/30*100, 3) 
newformat$mid_occ<-round(newformat$n/355*100, 3) 
newformat$hwSL_occ<-round(newformat$n/99*100,3)
newformat$hwS_occ<-round(newformat$n/370*100, 3)
newformat$drLL_occ<-round(newformat$n/194*100, 3)
newformat$drL_occ<-round(newformat$n/192*100, 3)
newformat$hwL_occ<-round(newformat$n/73*100, 3)
newformat$iso_occ<-round(newformat$n/100*100, 3)

