#library 
library(tidyr)

#load data
lake_dat<-read.csv('Datasets/lakesall.csv')
stream_dat<-read.csv('Datasets/streams_subset.csv')


#use subset of streams and lakes 
lk_subset<-dplyr::select(lakes_subset, LAKE_ID)
lake_dat<-left_join(lk_subset, lake_dat)

#combine datasets ##
L_dat<-dplyr::select(lake_dat, LAKE_ID, SPP_CODE_new, TOTAL_COUNT, conn_class, State_Name) 
S_dat<-dplyr::select(stream_dat, COMIDv2_JR, SPP_CODE_new, ind_count, CLASS, STATE) 
data.table::setnames(S_dat, old=c("COMIDv2_JR", "ind_count"), new=c("SITE_ID", "TOTAL_COUNT"))
data.table::setnames(L_dat, old=c("LAKE_ID", "conn_class", "State_Name"), new=c("SITE_ID", "CLASS", "STATE"))
L_dat$Type<-"lake"
S_dat$Type<-"stream"
S_dat$SITE_ID<-as.character(as.integer(S_dat$SITE_ID))

L_dat <- L_dat %>%
  mutate(CLASS = case_when(CLASS == 'DR_LakeStream' ~ 'DR_lake_lake',   #if ~ then 
                           CLASS == 'DR_Stream' ~ 'DR_lake',
                           CLASS == 'Headwater' ~ 'HW_lake',
                           CLASS == 'Isolated' ~ 'ISO_lake',
                              )) 
L_dat$CLASS<-as.factor(L_dat$CLASS)

L_dat <- L_dat %>%
  mutate(STATE = case_when(STATE == 'Iowa' ~ 'IA',   #if ~ then 
                                STATE == 'Maine' ~ 'ME',
                                STATE == 'Michigan' ~ 'MI',
                                STATE == 'Minnesota' ~ 'WI',
                                STATE == 'New Hampshire' ~ 'NH',
                                STATE == 'OUT_OF_COUNTY_STATE' ~ 'IA',
                                STATE == 'Wisconsin' ~ 'WI'
                                
  )) 
L_dat$STATE<-as.factor(L_dat$STATE)

#### remove species with < 5% occurrence rates ####
lakes2<-subset(L_dat, select = -c(CLASS, Type, STATE)) 
streams2<-subset(S_dat, select = -c(CLASS, Type, STATE)) 

lakenewformat<-pivot_wider(data= lakes2, #559
                           names_from = SPP_CODE_new, 
                           values_from = TOTAL_COUNT, 
                           values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                           values_fn = list(TOTAL_COUNT = sum)) 

lakenewformat<-subset(lakenewformat, select = -c(SITE_ID))
lakenewformat[lakenewformat > 0] <- 1 
L_col.sums<-stack(colSums(lakenewformat)/559*100) #sum the number of occurrences and divide by total number of lakes sampled
common_lake<-filter(L_col.sums, values > 5)  #38 with 5% 
L_subsp<-dplyr::left_join(common_lake, L_dat, by = c("ind" = "SPP_CODE_new"))
names(L_subsp)[names(L_subsp) == "ind"] <- "SPP_CODE_new"


streamnewformat<-pivot_wider(data= streams2, #854
                             names_from = SPP_CODE_new, 
                             values_from = TOTAL_COUNT, 
                             values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                             values_fn = list(TOTAL_COUNT = sum)) 
streamnewformat<-subset(streamnewformat, select = -c(SITE_ID))
streamnewformat[streamnewformat > 0] <- 1 

S_col.sums<-stack(colSums(streamnewformat)/854*100) #sum the number of occurrences and divide by total number of lakes sampled
common_stream<-filter(S_col.sums, values > 5)  #48 with 5% 
S_subsp<-dplyr::left_join(common_stream, S_dat, by = c("ind" = "SPP_CODE_new"))
names(S_subsp)[names(S_subsp) == "ind"] <- "SPP_CODE_new"

allecos<-gtools::smartbind(L_dat, S_dat) 
alleco_subsp<-gtools::smartbind(L_subsp, S_subsp) 

#62 common sp
distinct(alleco_subsp, SPP_CODE_new)


#### Correspondence Analysis (CA) ####
library("FactoMineR")
library("factoextra")

# organize data presence absence 
alleco_subsp$CLASS<-ordered(alleco_subsp$CLASS, levels=c('ISO_lake', 'HW_lake', 'DR_lake', 'DR_lake_lake',	'HW_stream', 'MID_stream', 'RIVER'))

fish2<-subset(alleco_subsp, select = -c(CLASS, Type, values, STATE)) 
allnewformat<-pivot_wider(data= fish2, 
                          names_from = SPP_CODE_new, 
                          values_from = TOTAL_COUNT, 
                          values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                          values_fn = list(TOTAL_COUNT = sum)) #adds up abundances if more than one occurence of a species is within a waterbody
presabs <- subset( allnewformat, select = -SITE_ID )
presabs[presabs > 0] <- 'yes'    #### could try occurrences?
presabs[presabs == 0] <- 'no' 
presabs[,] <- data.frame(apply(presabs[,], 2, as.factor))
summary(presabs)[, 1:4]
for (i in 1:4) {
  plot(presabs[,i], main=colnames(presabs)[i],
       ylab = "Count", col="steelblue", las = 2)
}

# need conny ids 
names<-dplyr::select(allnewformat, SITE_ID)
connyclass<-dplyr::select(alleco_subsp, SITE_ID, CLASS, STATE)
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
Fig5<-fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = connyids$CLASS, # color by groups 
             addEllipses = TRUE,
             palette = c("#1b9e77", "#e6ab02",  "#7570b3", "#e7298a",  "#d95f02", "#66a61e", "#a6761d"),
             ggtheme = theme_minimal(), 
             title = '') 

cowplot::save_plot("Figures/Fig5.png", Fig5, base_width = 7,
                   base_height = 5, dpi=600)


#add in ellipse base on states
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = connyids$STATE, # color by groups 
             addEllipses = TRUE, 
             ggtheme = theme_minimal(), 
             title = '') 


#### occurrence rates by class for the supp table ####
DR_LL<-filter(L_dat, CLASS== "DR_lake_lake")
DR_L<-filter(L_dat, CLASS== "DR_lake")
HW_L<-filter(L_dat, CLASS== "HW_lake")
ISO_L<-filter(L_dat, CLASS== "ISO_lake")
HW_S<-filter(S_dat, CLASS== "HW_stream")
MID_S<-filter(S_dat, CLASS== "MID_stream")
RIVER<-filter(S_dat, CLASS== "RIVER")

hw_sp<-as.data.frame(unique(HW_S$SPP_CODE_new))
mid_sp<-as.data.frame(unique(MID_S$SPP_CODE_new))
riv_sp<-as.data.frame(unique(RIVER$SPP_CODE_new))
iso_sp<-as.data.frame(unique(ISO_L$SPP_CODE_new))
hwL_sp<-as.data.frame(unique(HW_L$SPP_CODE_new))
drL_sp<-as.data.frame(unique(DR_L$SPP_CODE_new))
drLL_sp<-as.data.frame(unique(DR_LL$SPP_CODE_new))

dat2<-subset(MID_S, select = -c(CLASS, Type)) 

lakenewformat<-tidyr::pivot_wider(data= dat2, 
                                  names_from = SPP_CODE_new, 
                                  values_from = TOTAL_COUNT, 
                                  values_fill = list(TOTAL_COUNT = 0), #replace NAs with 0
                                  values_fn = list(TOTAL_COUNT = sum)) 
lakenewformat<-subset(lakenewformat, select = -c(SITE_ID))
lakenewformat[lakenewformat > 0] <- 1 

riv_col.sums<-stack(round(colSums(lakenewformat)/30*100), 3)
midS_col.sums<-stack(round(colSums(lakenewformat)/355*100), 3) 
hwS_col.sums<-stack(round(colSums(lakenewformat)/469*100), 3)
drLL_col.sums<-stack(round(colSums(lakenewformat)/194*100), 3)
drL_col.sums<-stack(round(colSums(lakenewformat)/192*100), 3)
hwL_col.sums<-stack(round(colSums(lakenewformat)/73*100), 3)
isoL_col.sums<-stack(round(colSums(lakenewformat)/100*100), 3)

write.csv(riv_col.sums, "/Users/katelynking/desktop/riv.csv", row.names=FALSE)