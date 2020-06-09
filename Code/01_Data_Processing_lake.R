#### Chap 2: Fish diversity across lakes and streams at the regional scale
##Written by Katelyn King July 15, 2018
## Updated: May 2019

#load libraries 
#install.packages('rgdal')
library(rgdal)
library(sp)
library(dplyr)
library(raster)
library(ggplot2)
library(tidyr)

#------ Step 1: QA/QC fish data -------

# Read in lake fish data from MI, Wisconsin, Iowa, NH, maine 
MIdata <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/MI_data.csv", header = TRUE) #98 lakes to start #2003-2006
WIdatan <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/wi_fish_net.csv", header = TRUE) # 468 lakes  
WIdatas <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/wi_fish_s.csv", header = TRUE)
IAdata <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/iowa_data.csv", header = TRUE) #127 lakes 
NHMEdata <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/NHME_data.csv", header = TRUE) #89 lakes to start
WI_info <- read.csv("/Users/katelynking/Desktop/Chap 2 Fish/fish lakes/WI_DNR_v3.csv", header = TRUE) #contains lat/lon
WI_info2<-dplyr::select(WI_info, CALC_LL_LAT_DD_AMT, CALC_LL_LONG_DD_AMT, WBIC, OFFICIAL_NAME )
WI_info2<-WI_info2[!duplicated(paste(WI_info2$WBIC)),]
names(WI_info2)[1]<-"LAT_DD"
names(WI_info2)[2]<-"LONG_DD"

### ### ### ### ### ### ### 
### clean up MI data set ###

#look at species list 
MIsp<-MIdata[!duplicated(paste(MIdata$SPP_CODE)),] %>% 
        dplyr::select(SPP_CODE)

#get rid of hybrid and unknown species; # Splake is a hybrid; smelt and mudminnow are not to the species level. 

MI_d<-filter(MIdata, !(SPP_CODE == "hybrid_sunfish" | SPP_CODE == "musky_hybrid" | 
                            SPP_CODE == "uknown_minnow" | SPP_CODE == "uknown_sucker" |
                            SPP_CODE == "unknown_bullhead" | SPP_CODE == "unknown_killifish" |
                            SPP_CODE == "unknown_redhorse" | SPP_CODE == "unknown_sculpin" | 
                         SPP_CODE == "smelt" | SPP_CODE == "splake" |  SPP_CODE == "mudminnow"))

#clean up species names across states 
MI_d$SPP_CODE_CAP<-casefold(MI_d$SPP_CODE, upper = TRUE) #make them all cap
MI_d$SPP_CODE_n<-gsub(" ", "_", MI_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space

MI_d <- MI_d %>%
  mutate(SPP_CODE_new = case_when(SPP_CODE_n == 'NORTHERN_HOGSUCKER' ~ 'NORTHERN_HOG_SUCKER',   #if ~ then 
                          SPP_CODE_n == 'ROCKBASS' ~ 'ROCK_BASS',
                          SPP_CODE_n == 'MUSKY' ~ 'MUSKELLUNGE',
                          TRUE ~ MI_d$SPP_CODE_n))  ### the else part 

MIsp<-MI_d [!duplicated(paste(MI_d $SPP_CODE_new)),] # 77 unique species 

### ### ### ### ### ### ### ### 
### clean up WI data set ###
WIdata1<-dplyr::select(WIdatan, County, Waterbody_Name, LAKE_CODE, YEAR, Sample_Date, GEAR, SPP_CODE, Sum_Fish_Count)
WIyear1<-filter(WIdata1, YEAR == "2001" | YEAR == "2002" | YEAR == "2003"| YEAR == "2004" |YEAR == "2005")

WIdata2<-dplyr::select(WIdatas, County, Waterbody_Name, LAKE_CODE, YEAR, Sample_Date, GEAR, SPP_CODE, Number_of_Fish)
WIyear2<-filter(WIdata2, YEAR == "2001" | YEAR == "2002" | YEAR == "2003"| YEAR == "2004" |YEAR == "2005")
names(WIyear2)[8]<-"Sum_Fish_Count"

#join the two tables
WIdata<-gtools::smartbind(WIyear1, WIyear2)
WIlakes<-WIdata[!duplicated(paste(WIdata$LAKE_CODE)),] # 468 lakes  
WIsp<-WIdata[!duplicated(paste(WIdata$SPP_CODE)),] 

# remove hybrid species and unknown species 
WI_d<-filter(WIdata, !(SPP_CODE == "BLUEGILL_X_UNKNOWN"  | SPP_CODE == "GREEN_SUNFISH_X_BLUEGILL" | SPP_CODE == "GREEN_SUNFISH_X_BLUEGILL_X_PUMPKINSEED" |
                            SPP_CODE == 'GREEN_SUNFISH_X_ORANGESPOTTED_SUNFISH' | SPP_CODE == "GREEN_SUNFISH_X_PUMPKINSEED" | SPP_CODE == "NO_FISH_CAPTURED" |
                            SPP_CODE == "NORTHERN_PIKE_X_MUSKELLUNGE" | SPP_CODE == "PUMPKINSEED_X_BLUEGILL" | SPP_CODE == "PUMPKINSEED_X_UNKNOWN" | 
                          SPP_CODE == "WARMOUTH_X_BLUEGILL" | SPP_CODE == "ALL_SPECIES" | SPP_CODE== "RUSTY_CRAYFISH" | SPP_CODE == "STICKLEBACKS_UNSP." |
                            SPP_CODE == "KILLIFISHES_UNSP." | SPP_CODE == "GARS_UNSP." | SPP_CODE == "CENTRARCHIDS" | SPP_CODE == "CENTRARCHID-PANFISH" | SPP_CODE == "PANFISH" |
                            SPP_CODE== "PANFISH_-_GAMEFISH" | SPP_CODE == "BULLHEAD_CATFISHES_UNSP." | SPP_CODE == "BULLHEADS" | SPP_CODE == "REDHORSES" | SPP_CODE == "SHINERS" | 
                         SPP_CODE == "MINNOWS_&_CARPS_UNSP." | SPP_CODE == "SCULPINS" | SPP_CODE == "CISCOES_&_WHITEFISHES" 
                       | SPP_CODE == "CRAPPIES" | SPP_CODE == "DACES" | SPP_CODE == "MADTOMS" |SPP_CODE == "LAMPREYS_UNSP."))

#clean up species names across states 
WI_d$SPP_CODE_CAP<-casefold(WI_d$SPP_CODE, upper = TRUE) #make them all cap
WI_d$SPP_CODE_n<-gsub(" ", "_", WI_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space
WIsp<-WI_d[!duplicated(paste(WI_d$SPP_CODE_n)),]  %>% 
  dplyr::select(SPP_CODE) # 96 unique species 

WI_d <- WI_d %>%
  mutate(SPP_CODE_new = case_when(SPP_CODE_n == 'CISCO_OR_LAKE_HERRING' ~ 'CISCO',   #if ~ then 
                                  SPP_CODE_n == 'TROUTPERCH' ~ 'TROUT_PERCH',
                                  SPP_CODE_n == 'AMERICAN_BROOK_LAMPREY_(AMMOCOETE)' ~ 'AMERICAN_BROOK_LAMPREY',
                                  TRUE ~ WI_d$SPP_CODE_n))  ### the else part 

summary(WI_d$GEAR)
WIsp<-WI_d[!duplicated(paste(WI_d$SPP_CODE_new)),] # 96 unique species 

### ### ### ### ### ### ### ### ### 
### clean up Iowa data set ### 
IAlakes<-IAdata[!duplicated(paste(IAdata$LAKE_CODE)),] # 127 unique lakes
IAsp<-IAdata[!duplicated(paste(IAdata$FISH_NAME)),] #investigate unique species

#get rid of hybrid and unknown species 
IA_d<-filter(IAdata, !(FISH_NAME == "Buffalo_Mixed_Species" | FISH_NAME == "Bullhead_Mixed_Species" |FISH_NAME == "Crappie_Mixed_Species" |
                            FISH_NAME == "Minnows_Mixed_Sps_&_families" | FISH_NAME == "Sauger_X_Walleye" | FISH_NAME == "Sucker,_Redhorse_Mixed" 
                          | FISH_NAME == "Sunfish_Mixed_Species" | FISH_NAME == "White_bass_X_Striped_Bass"))

#clean up species names across states 
IA_d$SPP_CODE_CAP<-casefold(IA_d$FISH_NAME, upper = TRUE) #make them all cap
IA_d$SPP_CODE_n<-gsub(" ", "_", IA_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space
IAsp<-IA_d[!duplicated(paste(IA_d$SPP_CODE_n)),] %>%
  dplyr::select(SPP_CODE_n)# 38 unique species

IA_d <- IA_d %>%
  mutate(SPP_CODE_new = case_when(SPP_CODE_n == 'QUILLBACK_CARPSUCKER' ~ 'QUILLBACK',   #if ~ then 
                                  TRUE ~ IA_d$SPP_CODE_n))  ### the else part 


summary(IA_d$GEAR)
IAsp<-IA_d[!duplicated(paste(IA_d$FISH_NAME)),] # 38 unique species

### ### ### ### ### ### ### ### 
### clean up NH/ME data set ### 
NHMEsp<-NHMEdata[!duplicated(paste(NHMEdata$NAME_COM)),] 

# remove other taxa (turtles, frogs, etc.), hybrid species, and unknown species  
NHME_d<-filter(NHMEdata, !(FISHCODE == "OTHER"  | FISHCODE == "OTHER1" | FISHCODE == "OTHER2" |
                                    FISHCODE == "OTHER3" | FISHCODE == "OTHER4" | FISHCODE == "OTHER5"| 
                                  FISHCODE == "HYBR01"  | FISHCODE == "HYBR02" |  FISHCODE == "UNKN02" | FISHCODE == "UNKN01" | 
                                    NAME_COM == "HYBRID PUMPXBLUEGILL" |
                                    NAME_COM == "HYBRID SUNFISH" | NAME_COM == "UNKNOWN" | NAME_COM == "UNKNOWN YOY" |
                                    NAME_COM == "UNKNOWN FISH" | NAME_COM == "POSS. HYBRID CENTRARCHID" |
                                    NAME_COM == "UNKNOWN CYPRINID" | NAME_COM == "UNKNOWN MINNOW" |
                                    NAME_COM == "PHOXINUS SPECIES" | NAME_COM == "LEPOMIS SPECIES"))

#Add up total adults, juveniles, and YOY
NHME_d$ADULT<-as.numeric(as.character(NHME_d$NUM_ADULT))
NHME_d$JUV<-as.numeric(as.character(NHME_d$NUM_JUVENILE))
NHME_d$YOY<-as.numeric(as.character(NHME_d$NUM_Y_O_Y))
NHME_d[is.na(NHME_d)] <- 0 
NHME_d$TOTAL_COUNT<- NHME_d$ADULT + NHME_d$JUV + NHME_d$YOY

#clean up species names across states 
NHME_d$SPP_CODE_CAP<-casefold(NHME_d$NAME_COM, upper = TRUE) #make them all cap
NHME_d$SPP_CODE_n<-gsub(" ", "_", NHME_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space
NHMEsp<-NHME_d[!duplicated(paste(NHME_d$SPP_CODE_n)),] %>%
  dplyr::select(SPP_CODE_n)# 50 unique species

NHME_d <- NHME_d %>%
  mutate(SPP_CODE_new = case_when(SPP_CODE_n == 'REDBREASTED_SUNFISH'~ 'REDBREAST_SUNFISH',   #if ~ then 
                                  TRUE ~ NHME_d$SPP_CODE_n))  ### the else part 

NHMEsp<-NHME_d[!duplicated(paste(NHME_d$SPP_CODE_new)),] # 49 unique species 

#filter out NH and Maine for summary stats
NH_d<-filter(NHME_d, grepl("NH", LAKE_ID, fixed = TRUE))
ME_d<-filter(NHME_d, grepl("ME", LAKE_ID, fixed = TRUE))
summary(NH_d$GEAR)
summary(ME_d$GEAR)  ## a large proportion don't have any gear listed :( 

#------ Step 2: rarefaction -------
library(vegan)
library(iNEXT)

#first look at within-lake success by gear-lake combinations
#combine some gear types 
MI_d$GEAR2<-sapply(MI_d$GEAR, function(x) {
  if(x == "BOOMSHK") {'SHOCK'}
  else {
    if(x == 'FYKENET') {'FYKE'}
    else {
      if(x == 'GLGNET') {'GILL'}
      else {
        if(x== 'IGNET') {'GILL'}
        else {
          if(x== 'MINIFYKE') {'FYKE'}
          else {
            if(x== 'SEINE') {'SEINE'}
            else {
              if(x== 'TRAPNET') {'FYKE'}
            }}}}}}})

#filter just columns we need and filter out unwanted Gear types 
MI_d<-filter(MI_d, GEAR2 == "SHOCK" | GEAR2 == "FYKE" )
MIdata1<-dplyr::select(MI_d, LAKE_CODE, GEAR2, SPP_CODE_new, Total_Number_Caught) 

#group by lake and gear and species to add up all the species for a gear 
MI4 <-MIdata1 %>% group_by(LAKE_CODE, GEAR2, SPP_CODE_new) %>% summarise(total=sum(Total_Number_Caught))

#make a new column called lake that is a combination of the lake code and gear type 
MI4$lake<-paste(MI4$LAKE_CODE, MI4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
MInewformat<-tidyr::spread(MI4, SPP_CODE_new, total) # aggregate by lake name and gear and adds up all sp. 
MInewformat[is.na(MInewformat)] <- 0 #NAs to 0 

MInewformat<-subset(MInewformat, select = -c(LAKE_CODE,GEAR2) )   #get rid of columns I do not want
row.names(MInewformat)<-MInewformat$lake  #lake names as a rows
fish2<-subset(MInewformat, select = -c(lake))   ##dataset to use for vegan code 

## run rarefaction via Vegan code 
### first create an empty data frame to save all of the output 
output_df <-data.frame(matrix(NA, nrow = length(MInewformat$lake), ncol = 3))
output_df[,1] = MInewformat$lake #put all the lake names in the first column
colnames(output_df) <- c("lake_gear","obs_spec", "slope")

for (i in 1:nrow(fish2)) { #this runs all rows
  sub<-fish2[i,] #takes each row
  obs<-specnumber(sub) #richness (observed)
  sample<- rowSums(sub) #sums all samples so you have the total number of individuals caught 
  slope <- rareslope(sub, sample=(sample*.9))  #90% of total indv. caught, slope we keep if <0.05    
  out3 <-round(slope, 3)
  output_df[i,2]<-obs #puts observed in the table 
  output_df[i,3]<-out3  ##this puts slope in the table 
}

output_MI<-output_df %>%  # data frame 
  tidyr::separate(lake_gear, c("LAKE_CODE", "GEAR"),  ## name of the column to separate and then each column 
           sep="-")  # seperate based on the dash symbol  

MIdata2<-left_join(MI_d, output_MI, by = c("LAKE_CODE", "GEAR2"="GEAR"))

no_plat<-filter(output_MI, slope > 0.05 | obs_spec == 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 7
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) ##7 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
MI_d2<-anti_join(MIdata2, bad_lakes)
MI_lakes_leftover<-MI_d2[!duplicated(paste(MI_d2$LAKE_CODE)),]
#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
MIdata_iNEXT<-dplyr::select(MI_d2, LAKE_CODE, SPP_CODE_new, Total_Number_Caught) %>% 
  group_by(LAKE_CODE, SPP_CODE_new) %>% summarise(total=sum(Total_Number_Caught)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(MIdata_iNEXT, LAKE_CODE, total) 

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE_new))  #dataset format to use for iNEXT code 

#run rafaction loop from iNEXT code 
lake_names<-MIdata_iNEXT[!duplicated(paste(MIdata_iNEXT$LAKE_CODE)),] # 91 unique lakes
MI_diversity_output <-data.frame(matrix(NA, nrow = length(lake_names$LAKE_CODE), ncol = 3))
MI_diversity_output[,1] = lake_names$LAKE_CODE #put all the lake names in the first column
colnames(MI_diversity_output) <- c("LAKE_CODE","exp_rich", "shannon")
for (i in 1:ncol(new)) {  #this runs all columns 
  out <- iNEXT(new[,i], q=0, datatype="abundance")    
  expected <-out$AsyEst[1,2]   #### select the species richness estimate 
  shannon <-out$AsyEst[2,2]  #### select the Shannon diversity estimate
  MI_diversity_output[i,2]<-expected ##this puts expected in the table  
  MI_diversity_output[i,3]<-shannon # put shannon value in the table 
}

## save output of clean datasets 
MI_data_QAQC<-left_join(MI_d2, MI_diversity_output, by="LAKE_CODE")
write.csv(MI_data_QAQC, 'Datasets/MI_data_QAQC.csv',row.names = FALSE)


## WISCONSIN
#change gear types 
WI_d$GEAR2<-sapply( WI_d$GEAR, function(x) {
  if(x == "MINI_FYKE_NET") {'FYKE'}
  else {
    if(x == 'MINI_FYKE_NET_WITH_UNKNOWN_TURTLE_EXCLUSION') {'FYKE'}
    else {
      if(x == 'MINI_FYKE_NET_WITH_TURTLE_EXCLUSION') {'FYKE'}
      else {
        if(x== 'MINI_FYKE_NET_WITHOUT_TURTLE_EXCLUSION') {'FYKE'}
        else {
          if(x== 'FYKE_NET') {'FYKE'}
        else {
          if(x== 'BACKPACK_SHOCKER') {'SHOCK'}
          else {
            if(x== 'MINI_BOOM_SHOCKER') {'SHOCK'}
            else {
              if(x== 'BOOM_SHOCKER') {'SHOCK'}
              else {
                if(x== 'STREAM_SHOCKER') {'SHOCK'}
                else {
                  if(x== 'SEINE') {'SEINE'}
                  else {
                    if(x== 'DROP_NET') {'DROP'}
      }}}}}}}}}}})

#filter to just columns we need and just gears 
WI_d<-filter(WI_d, GEAR2 == "SHOCK" | GEAR2 == "FYKE" )

WIdata1<-dplyr::select(WI_d, LAKE_CODE, GEAR2, SPP_CODE_new, Sum_Fish_Count) 

#group by lake and gear and species to add up all the species for a gear 
WI4 <-WIdata1 %>% group_by(LAKE_CODE, GEAR2, SPP_CODE_new) %>% summarise(total=sum(Sum_Fish_Count))

#make a new column called lake that is a combination of the lake code and gear type 
WI4$lake<-paste(WI4$LAKE_CODE, WI4$GEAR2, sep="-")  

#get rid of old lake name and gear type 
W<-subset(WI4, select = -c(LAKE_CODE, GEAR2))

#rearrange table to have species along the top for vegan package
WInewformat<-tidyr::spread(W, SPP_CODE_new, total) # aggregate by lake name and gear and adds up all sp. 
WInewformat[is.na(WInewformat)] <- 0 #NAs to 0 

row.names(WInewformat)<-WInewformat$lake  #lake names as a rows
fish2<-subset(WInewformat, select = -c(lake))   ##dataset to use for vegan code 

## run rarefaction code with vegan package
### first create an empty data frame to save all of the output 
output_df <-data.frame(matrix(NA, nrow = length(WInewformat$lake), ncol = 3))
output_df[,1] = WInewformat$lake
colnames(output_df) <- c("lake_gear","obs_spec", "slope")

for (i in 1:nrow(fish2)) { #this runs all rows
  sub<-fish2[i,] #takes each row
  obs<-specnumber(sub) #richness (observed)
  sample<- rowSums(sub) #sums all samples so you have the total number of individuals caught 
  slope <- rareslope(sub, sample=(sample*.9))  #90% of total indv. caught, slope we keep if <0.05    
  out3 <-round(slope, 3)
  output_df[i,2]<-obs #puts observed in the table 
  output_df[i,3]<-out3  ##this puts slope in the table 
}

output_WI<-output_df %>%  # data frame 
  separate(lake_gear, c("LAKE_CODE", "GEAR"),  ## name of the column to separate and then each column 
           sep="-")  # seperate based on the dash symbol

WI_d$LAKE_CODE<-as.factor(WI_d$LAKE_CODE)
WIdata2<-left_join(WI_d, output_WI, by = c("LAKE_CODE", "GEAR2"="GEAR"))

no_plat<-filter(output_WI, slope > 0.05 | obs_spec == 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 25
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) ##24 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
WI_d2<-anti_join(WIdata2, bad_lakes)
WI_lakes_leftover<-WI_d2[!duplicated(paste(WI_d2$LAKE_CODE)),] #444 lakes

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
WIdata_iNEXT<-dplyr::select(WI_d2, LAKE_CODE, SPP_CODE_new, Sum_Fish_Count) %>% 
  group_by(LAKE_CODE, SPP_CODE_new) %>% summarise(total=sum(Sum_Fish_Count)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(WIdata_iNEXT, LAKE_CODE, total) #442 lakes left and 104 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE_new))  #dataset format to use for iNEXT code 

#run rafaction loop from iNEXT code 
lake_names<-WIdata_iNEXT[!duplicated(paste(WIdata_iNEXT$LAKE_CODE)),] # 443 unique lakes
WI_diversity_output <-data.frame(matrix(NA, nrow = length(lake_names$LAKE_CODE), ncol = 3))
WI_diversity_output[,1] = lake_names$LAKE_CODE #put all the lake names in the first column
colnames(WI_diversity_output) <- c("LAKE_CODE","exp_rich", "shannon")
for (i in 1:ncol(new)) {  #this runs all columns 
  out <- iNEXT(new[,i], q=0, datatype="abundance")    
  expected <-out$AsyEst[1,2]   #### select the species richness estimate 
  shannon <-out$AsyEst[2,2]  #### select the Shannon diversity estimate
  WI_diversity_output[i,2]<-expected ##this puts expected in the table  
  WI_diversity_output[i,3]<-shannon # put shannon value in the table 
}

## save output of clean datasets 
WI_data_QAQC<-left_join(WI_d2, WI_diversity_output)
write.csv(WI_data_QAQC, 'Datasets/WI_data_QAQC.csv',row.names = FALSE)


## IOWA 
#make gear names the same as other states
IA_d$GEAR2<-sapply(IA_d$GEAR, function(x) {
  if(x == "fyke") {'FYKE'}
  else {
    if(x == 'Shock') {'SHOCK'}
  }})
    
#filter to just columns we need
IAdata1<-dplyr::select(IA_d, LAKE_CODE, GEAR2, SPP_CODE_new, TOTAL_COUNT) 

#group by lake and gear and species to add up all the species for a gear 
IA4 <-IAdata1 %>% group_by(LAKE_CODE, GEAR2, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))

#make a new column called lake that is a combination of the lake code and gear type 
IA4$lake<-paste(IA4$LAKE_CODE, IA4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
IAnewformat<-tidyr::spread(IA4, SPP_CODE_new, total) # aggregate by lake name and gear and adds up all sp. 
IAnewformat[is.na(IAnewformat)] <- 0 #NAs to 0 

IAnewformat<-subset(IAnewformat, select = -c(LAKE_CODE,GEAR2) )   #get rid of columns I do not want
row.names(IAnewformat)<-IAnewformat$lake  #lake names as a rows
fish2<-subset(IAnewformat, select = -c(lake))   ##dataset to use for vegan code 

## run rarefaction code 
### first create an empty data frame to save all of the output 
output_df <-data.frame(matrix(NA, nrow = length(IAnewformat$lake), ncol = 3))
output_df[,1] = IAnewformat$lake
colnames(output_df) <- c("lake_gear","obs_spec", "slope")

for (i in 1:nrow(fish2)) { #this runs all rows
  sub<-fish2[i,] #takes each row
  obs<-specnumber(sub) #richness (observed)
  sample<- rowSums(sub) #sums all samples so you have the total number of individuals caught 
  slope <- rareslope(sub, sample=(sample*.9))  #90% of total indv. caught, slope we keep if <0.05    
  out3 <-round(slope, 3)
  output_df[i,2]<-obs #puts observed in the table 
  output_df[i,3]<-out3  ##this puts slope in the table 
}

output_IA<-output_df %>%  # data frame 
  separate(lake_gear, c("LAKE_CODE", "GEAR"),  ## name of the column to separate and then each column 
           sep="-")  # seperate based on the dash symbol

IA_d$LAKE_CODE<-as.factor(IA_d$LAKE_CODE)
IAdata2<-left_join(IA_d, output_IA, by = c("LAKE_CODE", "GEAR2"="GEAR"))

no_plat<-filter(output_IA, slope > 0.05 | obs_spec == 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) #9 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
IA_d2<-anti_join(IAdata2, bad_lakes)
IA_lakes_leftover<-IA_d2[!duplicated(paste(IA_d2$LAKE_CODE)),] #118 lakes

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
IAdata_iNEXT<-dplyr::select(IA_d2, LAKE_CODE, SPP_CODE_new, TOTAL_COUNT) %>% 
  group_by(LAKE_CODE, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(IAdata_iNEXT, LAKE_CODE, total) #118 lakes left and 38 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE_new))  #dataset format to use for iNEXT code 

#use iNEXT to estimate expected number of species   
#run rafaction loop from iNEXT code 
lake_names<-IAdata_iNEXT[!duplicated(paste(IAdata_iNEXT$LAKE_CODE)),] # 118 unique lakes
IA_diversity_output <-data.frame(matrix(NA, nrow = length(lake_names$LAKE_CODE), ncol = 3))
IA_diversity_output[,1] = lake_names$LAKE_CODE #put all the lake names in the first column
colnames(IA_diversity_output) <- c("LAKE_CODE","exp_rich", "shannon")
for (i in 1:ncol(new)) {  #this runs all columns 
  out <- iNEXT(new[,i], q=0, datatype="abundance")    
  expected <-out$AsyEst[1,2]   #### select the species richness estimator 
  shannon <-out$AsyEst[2,2]  #### select the Shannon diversity estimate
  IA_diversity_output[i,2]<-expected ##this puts expected in the table  
  IA_diversity_output[i,3]<-shannon # put shannon value in the table 
  }

## save output of clean datasets 
IA_data_QAQC<-left_join(IA_d2, IA_diversity_output )
write.csv(IA_data_QAQC, 'Datasets/IA_data_QAQC.csv',row.names = FALSE)

## NHME
#change gear types 
NHME_d$GEAR2<-sapply(NHME_d$GEAR, function(x) {
  if(x == 'B') {'SEINE'}
  else {
    if(x == 'S') {'SEINE'}
    else {
      if(x == 'T') {'FYKE'}
      else {
        if(x== 'P') {'FYKE'}
        else {
          if(x== 'G') {'GILL'}
          else {
            if(x== 'E') {'SHOCK'}
            else {
              if(x== 'M') {'MINNOW'}
              else {
                if(x== 'N') {'N'}
                else {("NA")}
                    }}}}}}}})

#filter to just columns we need and just gear we need 
NHME_d<-filter(NHME_d, GEAR2 == "SHOCK" | GEAR2 == "FYKE" | GEAR2 == "NA")
NHMEdata1<-dplyr::select(NHME_d, LAKE_ID, GEAR2, SPP_CODE_new, TOTAL_COUNT) 

#group by lake and gear and species to add up all the species for a gear 
NHME4 <-NHMEdata1 %>% group_by(LAKE_ID, GEAR2,  SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT))

#make a new column called lake that is a combination of the lake code and gear type 
NHME4$lake<-paste(NHME4$LAKE_ID, NHME4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
NHMEnewformat<-tidyr::spread(NHME4, SPP_CODE_new, total) # aggregate by lake name and gear and adds up all sp. 
NHMEnewformat[is.na(NHMEnewformat)] <- 0 #NAs to 0 

NHMEnewformat<-subset(NHMEnewformat, select = -c(LAKE_ID,GEAR2) )   #get rid of columns I do not want
row.names(NHMEnewformat)<-NHMEnewformat$lake  #lake names as a rows
fish2<-subset(NHMEnewformat, select = -c(lake))   ##dataset to use for vegan code 

## run rarefaction code 
### first create an empty data frame to save all of the output 
output_df <-data.frame(matrix(NA, nrow = length(NHMEnewformat$lake), ncol = 3))
output_df[,1] = NHMEnewformat$lake
colnames(output_df) <- c("lake_gear","obs_spec", "slope")

for (i in 1:nrow(fish2)) { #this runs all rows
  sub<-fish2[i,] #takes each row
  obs<-specnumber(sub) #richness (observed)
  sample<- rowSums(sub) #sums all samples so you have the total number of individuals caught 
  slope <- rareslope(sub, sample=(sample*.9))  #90% of total indv. caught, slope we keep if <0.05    
  out3 <-round(slope, 3)
  output_df[i,2]<-obs #puts observed in the table 
  output_df[i,3]<-out3  ##this puts slope in the table 
}


output_NHME<-output_df %>%  # data frame 
  separate(lake_gear, c("LAKE_CODE", "GEAR"),  ## name of the column to separate and then each column 
           sep="-")  # seperate based on the dash symbol

NHMEdata2<-left_join(NHME_d, output_NHME, by = c("LAKE_ID" ="LAKE_CODE", "GEAR2"="GEAR"))

no_plat<-filter(output_NHME, slope > 0.05 | obs_spec == 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) #9 lakes
# select the observations from the original dataset minus gears that only collected one sp. and remove waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
NHME_d2<-anti_join(NHMEdata2, bad_lakes, by = c("LAKE_ID" ="LAKE_CODE"))
NHME_lakes_leftover<-NHME_d2[!duplicated(paste(NHME_d2$LAKE_ID)),] 

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
NHMEdata_iNEXT<-dplyr::select(NHME_d2, LAKE_ID, SPP_CODE_new, TOTAL_COUNT) %>% 
  group_by(LAKE_ID, SPP_CODE_new) %>% summarise(total=sum(TOTAL_COUNT)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(NHMEdata_iNEXT, LAKE_ID, total) 

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE_new))  #dataset format to use for iNEXT code 

#use iNEXT to estimate expected number of species   
#run rafaction loop from iNEXT code 
lake_names<-NHMEdata_iNEXT[!duplicated(paste(NHMEdata_iNEXT$LAKE_ID)),] # 79 unique lakes
NHME_diversity_output <-data.frame(matrix(NA, nrow = length(lake_names$LAKE_ID), ncol = 3))
NHME_diversity_output[,1] = lake_names$LAKE_ID #put all the lake names in the first column
colnames(NHME_diversity_output) <- c("LAKE_CODE","exp_rich", "shannon")

#use iNEXT to estimate expected number of species   
for (i in 1:ncol(new)) {  #this runs all columns 
  out <- iNEXT(new[,i], q=0, datatype="abundance")    
  expected <-out$AsyEst[1,2]   #### select the species richness estimator 
  shannon <-out$AsyEst[2,2]  #### select the Shannon diversity estimate
  NHME_diversity_output[i,2]<-expected ##this puts expected in the table  
  NHME_diversity_output[i,3]<-shannon # put shannon value in the table 
}

## save output of clean datasets 
NHME_data_QAQC<-left_join(NHME_d2, NHME_diversity_output, by = c("LAKE_ID" ="LAKE_CODE"))
write.csv(NHME_data_QAQC, 'Datasets/NHME_data_QAQC.csv',row.names = FALSE)

#filter out NH and Maine for summary stats
NH_d<-filter(NHME_diversity_output, grepl("NH", LAKE_CODE, fixed = TRUE)) #26 lakes 
ME_d<-filter(NHME_diversity_output, grepl("ME", LAKE_CODE, fixed = TRUE)) #54 lakes 


#------ Step 3: Merge fish data with LAGOS -------
# Read SHAPEFILE.shp from the LAGOS database 
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/LAGOS_NE_All_Lakes_4ha", layer = "LAGOS_NE_All_Lakes_4ha")
crs(lakes_shape) #shows me the projection of the shapefiles so that I can project the same to the points 
prjnew <- CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m
              +no_defs +ellps=GRS80 +towgs84=0,0,0')

#project data 
IA<-read.csv('Datasets/IA_data_QAQC.csv')
MI<-read.csv('Datasets/MI_data_QAQC.csv')
NHME<-read.csv('Datasets/NHME_data_QAQC.csv')
WI<-read.csv('Datasets/WI_data_QAQC.csv')

MIpoints<-dplyr::select(MI, LAKE_CODE, LAT_DD, LONG_DD)
MIpoints<- MIpoints[!duplicated(paste(MIpoints$LAKE_CODE)),] #select only one row of each lake
MIpoints<-MIpoints[!(is.na(MIpoints$LAT_DD)),] #get rid of lakes that don't have coords
x <- MIpoints$LONG_DD
y <- MIpoints$LAT_DD

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   
MI.aea <- spTransform(lake.ll, prjnew)
plot(MI.aea)
MI_coords<-as.data.frame(MI.aea)

### LINK TO LAGOS ### 
#use the over function to match points to LAGOS polygons. 
#Returns a data.frame of the second argument with row entries corresponding to the first argument
LAGOS.MI.match<-sp::over(MI.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
LAGOS.MI.match$LAKE_CODE<-paste(MIpoints$LAKE_CODE) # 2 don't have coords  
LAGOS.MI.ids<-dplyr::select(LAGOS.MI.match, LAKE_CODE, Permanent_, lagoslakei, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name, AreaSqKm) #2 lakes don't match to LAGOS (left with 87 lakes)

#project Wisconsin Data 
WI_info2$WBIC2<-as.character(WI_info2$WBIC)
WIdataLL<-left_join(WI, WI_info2, by = c("LAKE_CODE" = "WBIC")) ## join the lat lon info 
WIpoints<-dplyr::select(WIdataLL, LAKE_CODE, LAT_DD, LONG_DD)
WIpoints<- WIpoints[!duplicated(paste(WIpoints$LAKE_CODE)),] #select only one row of each lake
WIpoints<-WIpoints[!(is.na(WIpoints$LAT_DD)),] #get rid of lakes that don't have lat/long (down to 424)
x <- WIpoints$LONG_DD
y <- WIpoints$LAT_DD

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   
WI.aea <- spTransform(lake.ll, prjnew)
plot(WI.aea)

### LINK TO LAGOS ### 
#use the over function to match points to LAGOS polygons. 
#Returns a data.frame of the second argument with row entries corresponding to the first argument
LAGOS.WI.match<-sp::over(WI.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
LAGOS.WI.match$LAKE_CODE<-paste(WIpoints$LAKE_CODE) 
LAGOS.WI.ids<-dplyr::select(LAGOS.WI.match, LAKE_CODE, Permanent_, lagoslakei, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name, AreaSqKm) #down to 340 lakes that match to LAGOS

#Project Iowa data 
IApoints<-dplyr::select(IA, LAKE_CODE, UTM_NAD83__N, UTM_NAD83__E)
IApoints<- IApoints[!duplicated(paste(IApoints$LAKE_CODE)),] #select only one row of each lake
x <- IApoints$UTM_NAD83__E
y <- IApoints$UTM_NAD83__N

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=utm +zone=15 +datum=NAD83")
lake.utm <- SpatialPoints(coords, proj4string=prj)   
IA.aea <- spTransform(lake.utm, prjnew)
plot(IA.aea)
#convert to lat/lon to match other States
IA.ll <- spTransform(lake.utm, CRS("+proj=longlat +datum=WGS84"))
IA_coords<-as.data.frame(IA.ll)
plot(IA.ll)
IA_coords$LAKE_CODE = IApoints$LAKE_CODE

### LINK TO LAGOS ### 
#use the over function to match points to LAGOS polygons. 
#Returns a data.frame of the second argument with row entries corresponding to the first argument
LAGOS.IA.match<-sp::over(IA.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
LAGOS.IA.match$LAKE_CODE<-paste(IApoints$LAKE_CODE) 
LAGOS.IA.ids<-dplyr::select(LAGOS.IA.match, LAKE_CODE, Permanent_, lagoslakei, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name, AreaSqKm) #down to 98 lakes taht match to LAGOS

#Project NH/ME data 
NHMEpoints<-dplyr::select(NHME, LAKE_ID, LAT_DD, LON_DD)
NHMEpoints<- NHMEpoints[!duplicated(paste(NHMEpoints$LAKE_ID)),] #select only one row of each lake
x <- NHMEpoints$LON_DD
y <- NHMEpoints$LAT_DD

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   
NHME.aea <- spTransform(lake.ll, prjnew)
plot(NHME.aea)

### LINK TO LAGOS ### 
#use the over function to match points to LAGOS polygons. 
#Returns a data.frame of the second argument with row entries corresponding to the first argument
LAGOS.NHME.match<-sp::over(NHME.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
LAGOS.NHME.match$LAKE_CODE<-paste(NHMEpoints$LAKE_ID) 
LAGOS.NHME.ids<-dplyr::select(LAGOS.NHME.match, LAKE_CODE, Permanent_, lagoslakei, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name, AreaSqKm) #down to 74 lakes that match to LAGOS

LAGOSall<-gtools::smartbind(LAGOS.MI.ids, LAGOS.WI.ids, LAGOS.IA.ids, LAGOS.NHME.ids)
LAGOSall$lagoslakeid<-as.integer(as.character(LAGOSall$lagoslakei))

## Classify as lake or reservoir ###
#res<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/reservoir data/res_class_man_all.csv", header=TRUE) %>%
#  dplyr::select(LAKE_TYPE, lagoslakeid)

#res_m<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/reservoir data/res_class_model_all.csv", header=TRUE) %>%
#  dplyr::select(Prediction, lagoslakeid)
#names(res_m)[1]<-"LAKE_TYPE"

#res_lakes<-gtools::smartbind(res, res_m)

#res_class<-left_join(LAGOSall, res_lakes)

#res_class<-res_class[!(is.na(res_class$LAKE_TYPE)),]

#res_class$TYPE<-ifelse(res_class$LAKE_TYPE == 'NL', 'NL', 'RES' )
#IDs<-subset(res_class, select = -c(lagoslakei,LAKE_TYPE) )          

#join to data
MI_lagosid<-left_join(MI, LAGOSall)
WI$LAKE_ID<-as.character(WI$LAKE_CODE)
WI_lagosid<-left_join(WI, LAGOSall, by=c('LAKE_ID' = 'LAKE_CODE'))
IA$LAKE_ID<-as.character(IA$LAKE_CODE)
IA_lagosid<-left_join(IA, LAGOSall, by=c('LAKE_ID' = 'LAKE_CODE'))
NHME_lagosid<-left_join(NHME, LAGOSall, by=c('LAKE_ID' = 'LAKE_CODE'))

###add coords 
WI_lagos_ll<-left_join(WI_lagosid, WIpoints)
IA_lagos_ll<-left_join(IA_lagosid, IA_coords)

#### Combine datasets 
MI_com<-dplyr::select(MI_lagosid, LAKE_CODE, LAT_DD, LONG_DD, SPP_CODE_new, Total_Number_Caught, exp_rich, obs_spec, AreaSqKm, lagoslakeid, Permanent_, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name)
NHME_com<-dplyr::select(NHME_lagosid, LAKE_ID, LAT_DD, LON_DD, SPP_CODE_new, TOTAL_COUNT, exp_rich, obs_spec, AreaSqKm, lagoslakeid, Permanent_, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name)
WI_com<-dplyr::select(WI_lagos_ll, LAKE_ID, LAT_DD, LONG_DD, SPP_CODE_new, Sum_Fish_Count, exp_rich, obs_spec, AreaSqKm, lagoslakeid, Permanent_, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name)
IA_com<-dplyr::select(IA_lagos_ll, LAKE_ID, x, y, SPP_CODE_new, TOTAL_COUNT, lagoslakeid, exp_rich, obs_spec, AreaSqKm, Permanent_, HU4_ZoneID, HU6_ZoneID, HU8_ZoneID, HU12_ZoneI, State_Name)

names(MI_com)[1]<-"LAKE_ID"  #lake name = LAKE_ID
names(MI_com)[3]<-"LON_DD"
names(MI_com)[5]<-"TOTAL_COUNT"
names(WI_com)[3]<-"LON_DD"
names(WI_com)[5]<-"TOTAL_COUNT"
names(IA_com)[2]<-"LON_DD"
names(IA_com)[3]<-"LAT_DD"
IA_com<-IA_com[,c(1,3,2,4,5,6,7,8,9,10, 11, 12, 13, 14, 15)] # re-arrange columns 
lakesall<-gtools::smartbind(MI_com, IA_com, WI_com, NHME_com)
lakesall<-lakesall[!(is.na(lakesall$lagoslakeid)),]  # remove lakes that did not match to LAGOS 
lakecount<-lakesall[!duplicated(paste(lakesall$lagoslakeid)),] #588 lakes remaining 
allsp<-lakesall[!duplicated(paste(lakesall$SPP_CODE_new)),] %>%
        dplyr::select(SPP_CODE_new) #121

data.table::setnames(lakesall, old=c("HU12_ZoneI"), new=c("HU12_ZoneID"))

# Add connectivity class from LAGOS 
library(LAGOSNE)
lg <- lagosne_load("1.087.3") 
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        conn_class=lake_conn$lakeconnection)

lakesall<-left_join(lakesall, lake_conn)
write.csv(lakesall, ("Datasets/lakesall.csv"), row.names = FALSE)

#### lake subset #### 
# from the DR_LKST category select lake with the most species in a HUC12; would assume the other lake in the huc12 are a subset
lakesall<-read.csv("Datasets/lakesall.csv")
lakes_to_subsest<-lakesall[!duplicated(paste(lakesall$LAKE_ID)),] %>%
  dplyr::select(LAKE_ID, obs_spec, HU12_ZoneID, conn_class, lagoslakeid)

DR_lakeST <-filter(lakes_to_subsest, conn_class == "DR_LakeStream") 
other_lakes <-filter(lakes_to_subsest, conn_class == "Isolated" |  conn_class == "Headwater" | conn_class == "DR_Stream")

DRlakes_sub<-do.call(rbind, lapply(split(DR_lakeST, DR_lakeST$HU12_ZoneID), function(x) {return(x[which.max(x$obs_spec),])}))

lakes_subset<-gtools::smartbind(DRlakes_sub, other_lakes)
summary(as.factor(lakes_subset$conn_class))

