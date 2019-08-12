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
 #get rid of hybrid and unknown species 
MI_fish<-filter(MIdata, !(SPP_CODE == "hybrid_sunfish" | SPP_CODE == "musky_hybrid" | 
                            SPP_CODE == "uknown_minnow" | SPP_CODE == "uknown_sucker"))

# keep unknown killifish, redhorse, sculpin as unique species. keep unknown_bullhead in lakes where there are not other bullhead
MI_d<-filter(MI_fish, !(SPP_CODE == "unknown_bullhead" & LAKE_CODE == "24_71")) %>%
  filter( !(SPP_CODE == "unknown_bullhead" & LAKE_CODE == "31_585")) %>%
  filter( !(SPP_CODE == "unknown_bullhead" & LAKE_CODE == "4_4"))

summary(MI_d$GEAR)
MIsp<-MI_d[!duplicated(paste(MI_d$SPP_CODE)),] # 84 unique species
MIlakes<-MI_d[!duplicated(paste(MI_d$LAKE_CODE)),] # 98 unique lakes

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

# remove hybrid species and unknown species 
WI_fish<-filter(WIdata, !(SPP_CODE == "BLUEGILL_X_UNKNOWN"  | SPP_CODE == "GREEN_SUNFISH_X_BLUEGILL" | SPP_CODE == "GREEN_SUNFISH_X_BLUEGILL_X_PUMPKINSEED" |
                            SPP_CODE == 'GREEN_SUNFISH_X_ORANGESPOTTED_SUNFISH' | SPP_CODE == "GREEN_SUNFISH_X_PUMPKINSEED" | SPP_CODE == "NO_FISH_CAPTURED" |
                            SPP_CODE == "NORTHERN_PIKE_X_MUSKELLUNGE" | SPP_CODE == "PUMPKINSEED_X_BLUEGILL" | SPP_CODE == "PUMPKINSEED_X_UNKNOWN" | 
                          SPP_CODE == "WARMOUTH_X_BLUEGILL" | SPP_CODE == "ALL_SPECIES" | SPP_CODE== "RUSTY_CRAYFISH" | SPP_CODE == "STICKLEBACKS_UNSP." |
                            SPP_CODE == "KILLIFISHES_UNSP." | SPP_CODE == "GARS_UNSP." | SPP_CODE == "CENTRARCHIDS" | SPP_CODE == "CENTRARCHID-PANFISH" | SPP_CODE == "PANFISH" |
                            SPP_CODE== "PANFISH_-_GAMEFISH"))

#remove unknown species from lake if there is already a species in that lake 
WI_d<-filter(WI_fish, !(SPP_CODE == "BULLHEAD_CATFISHES_UNSP." & LAKE_CODE == "2453500")) %>%
  filter( !(SPP_CODE == "BULLHEADS" & LAKE_CODE== "540600")) %>%
  filter( !(SPP_CODE == "BULLHEADS" & LAKE_CODE == "609400"))  %>%
  filter( !(SPP_CODE == "BULLHEADS" & LAKE_CODE== "2489400")) %>%
  filter( !(SPP_CODE == "BULLHEADS" & LAKE_CODE == "2901700")) %>%
  filter( !(SPP_CODE == "BULLHEADS" & LAKE_CODE == "2743300")) %>%
  filter( !(SPP_CODE == "REDHORSES" & LAKE_CODE == "2268300")) %>%
  filter( !(SPP_CODE == "SHINERS" & LAKE_CODE == "1523000")) %>%
  filter( !(SPP_CODE == "SHINERS" & LAKE_CODE == "1653500")) %>%
  filter( !(SPP_CODE == "SHINERS" & LAKE_CODE == "2068000")) %>%
  filter( ! (SPP_CODE == "MINNOWS_&_CARPS_UNSP." & LAKE_CODE == "27500" )) %>%
  filter( ! (SPP_CODE == "MINNOWS_&_CARPS_UNSP." & LAKE_CODE == "45200" ))%>%
  filter( ! (SPP_CODE == "MINNOWS_&_CARPS_UNSP." & LAKE_CODE == "591600" ))%>%
  filter( ! (SPP_CODE == "MINNOWS_&_CARPS_UNSP." & LAKE_CODE == "739100" ))%>%
  filter( ! (SPP_CODE == "MINNOWS_&_CARPS_UNSP." & LAKE_CODE == "858300" ))

summary(WI_d$GEAR)
WIsp<-WI_d[!duplicated(paste(WI_d$SPP_CODE)),] # 107 unique species 

### ### ### ### ### ### ### ### ### 
### clean up Iowa data set ### 
IAlakes<-IAdata[!duplicated(paste(IAdata$LAKE_CODE)),] # 127 unique lakes

#get rid of hybrid and unknown species 
IA_d<-filter(IAdata, !(FISH_NAME == "Buffalo_Mixed_Species" | FISH_NAME == "Bullhead_Mixed_Species" |FISH_NAME == "Crappie_Mixed_Species" |
                            FISH_NAME == "Minnows_Mixed_Sps_&_families" | FISH_NAME == "Sauger_X_Walleye" | FISH_NAME == "Sucker,_Redhorse_Mixed" 
                          | FISH_NAME == "Sunfish_Mixed_Species" | FISH_NAME == "White_bass_X_Striped_Bass"))
summary(IA_d$GEAR)
IAsp<-IA_d[!duplicated(paste(IA_d$FISH_NAME)),] # 38 unique species

### ### ### ### ### ### ### ### 
### clean up NH/ME data set ### 
# remove other taxa (turtles, frogs, etc.), hybrid species, and unknown species  
NHMEdata_FISH<-filter(NHMEdata, !(FISHCODE == "OTHER"  | FISHCODE == "OTHER1" | FISHCODE == "OTHER2" |
                                    FISHCODE == "OTHER3" | FISHCODE == "OTHER4" | FISHCODE == "OTHER5"| 
                                  FISHCODE == "HYBR01"  | FISHCODE == "HYBR02" |  FISHCODE == "UNKN02" | NAME_COM == "HYBRID PUMPXBLUEGILL" |
                                    NAME_COM == "HYBRID SUNFISH" | NAME_COM == "UNKNOWN" | NAME_COM == "UNKNOWN YOY" |
                                    NAME_COM == "UNKNOWN FISH" | NAME_COM == "POSS. HYBRID CENTRARCHID" |
                                    NAME_COM == "UNKNOWN CYPRINID" | NAME_COM == "UNKNOWN MINNOW" |
                                    NAME_COM == "PHOXINUS SPECIES" | NAME_COM == "LEPOMIS SPECIES"))

#keep Leposmis Sp. as unique if no other Lepomis was found in the lake only NH500L  
NHME_d<-filter(NHMEdata_FISH, !(FISHCODE == "UNKN01" & LAKE_ID == "NH501L")) %>%
            filter( !(FISHCODE == "UNKN01" & LAKE_ID == "NH758L")) %>%
              filter( !(FISHCODE == "UNKN01" & LAKE_ID == "ME508L"))

#Add up total adults, juveniles, and YOY
NHME_d$ADULT<-as.numeric(as.character(NHME_d$NUM_ADULT))
NHME_d$JUV<-as.numeric(as.character(NHME_d$NUM_JUVENILE))
NHME_d$YOY<-as.numeric(as.character(NHME_d$NUM_Y_O_Y))
NHME_d[is.na(NHME_d)] <- 0 
NHME_d$TOTAL_COUNT<- NHME_d$ADULT + NHME_d$JUV + NHME_d$YOY

NHMEsp<-NHME_d[!duplicated(paste(NHME_d$NAME_COM)),] # 51 unique species 

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
#filter to just columns we need
MIdata1<-dplyr::select(MI_d, LAKE_CODE, GEAR2, SPP_CODE, Total_Number_Caught) 

#group by lake and gear and species to add up all the species for a gear 
MI4 <-MIdata1 %>% group_by(LAKE_CODE, GEAR2, SPP_CODE) %>% summarise(total=sum(Total_Number_Caught))

#make a new column called lake that is a combination of the lake code and gear type 
MI4$lake<-paste(MI4$LAKE_CODE, MI4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
MInewformat<-tidyr::spread(MI4, SPP_CODE, total) # aggregate by lake name and gear and adds up all sp. 
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

sep<-strsplit(output_df$lake, "-")
mat<-matrix(unlist(sep), ncol=2, byrow=TRUE)
df<- as.data.frame(mat)
colnames(df) <- c("LAKE_CODE", "GEAR") 
output_MI<- cbind(df, output_df)

MIdata2<-left_join(MI_d, output_MI, by = c("LAKE_CODE", "GEAR2"="GEAR"))

MIdata_lessgears<-filter(MIdata2, obs_spec > 1) #remove lake-gears with only one species 
no_plat<-filter(output_MI, slope > 0.05 & obs_spec > 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 35
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) ##35 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
MI_d2<-anti_join(MIdata_lessgears, bad_lakes)

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
MIdata_iNEXT<-dplyr::select(MI_d2, LAKE_CODE, SPP_CODE, Total_Number_Caught) %>% 
  group_by(LAKE_CODE, SPP_CODE) %>% summarise(total=sum(Total_Number_Caught)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(MIdata_iNEXT, LAKE_CODE, total) #62 lakes left and 78 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE))  #dataset format to use for iNEXT code 

#run rafaction loop from iNEXT code 
lake_names<-MIdata_iNEXT[!duplicated(paste(MIdata_iNEXT$LAKE_CODE)),] # 62 unique lakes
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
write.csv(MI_d2, 'Datasets/MI_data_QAQC.csv',row.names = FALSE)
write.csv(MI_diversity_output, 'Datasets/MI_div.csv', row.names = FALSE )

## WISCONSIN
#filter to just columns we need
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



WIdata1<-dplyr::select(WI_d, LAKE_CODE, GEAR2, SPP_CODE, Sum_Fish_Count) 

#group by lake and gear and species to add up all the species for a gear 
WI4 <-WIdata1 %>% group_by(LAKE_CODE, GEAR2, SPP_CODE) %>% summarise(total=sum(Sum_Fish_Count))

#make a new column called lake that is a combination of the lake code and gear type 
WI4$lake<-paste(WI4$LAKE_CODE, WI4$GEAR2, sep="-")  

#get rid of old lake name and gear type 
W<-subset(WI4, select = -c(LAKE_CODE, GEAR2))

#rearrange table to have species along the top for vegan package
WInewformat<-tidyr::spread(W, SPP_CODE, total) # aggregate by lake name and gear and adds up all sp. 
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

sep<-strsplit(output_df$lake, "-")
mat<-matrix(unlist(sep), ncol=2, byrow=TRUE)
df<- as.data.frame(mat)
colnames(df) <- c("LAKE_CODE", "GEAR") 
output_WI<- cbind(df, output_df)

WI_d$LAKE_CODE<-as.factor(WI_d$LAKE_CODE)
WIdata2<-left_join(WI_d, output_WI, by = c("LAKE_CODE", "GEAR2"="GEAR"))

WIdata_lessgears<-filter(WIdata2, obs_spec > 1) #remove lake-gears with only one species 
no_plat<-filter(output_WI, slope > 0.05 & obs_spec > 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 35
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) ##26 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
WI_d2<-anti_join(WIdata_lessgears, bad_lakes)

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
WIdata_iNEXT<-dplyr::select(WI_d2, LAKE_CODE, SPP_CODE, Sum_Fish_Count) %>% 
  group_by(LAKE_CODE, SPP_CODE) %>% summarise(total=sum(Sum_Fish_Count)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(WIdata_iNEXT, LAKE_CODE, total) #442 lakes left and 104 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE))  #dataset format to use for iNEXT code 

#run rafaction loop from iNEXT code 
lake_names<-WIdata_iNEXT[!duplicated(paste(WIdata_iNEXT$LAKE_CODE)),] # 442 unique lakes
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
write.csv(WI_d2, 'Datasets/WI_data_QAQC.csv',row.names = FALSE)
write.csv(WI_diversity_output, 'Datasets/WI_div.csv', row.names = FALSE )

## IOWA 
#make gear names the same as other states
IA_d$GEAR2<-sapply(IA_d$GEAR, function(x) {
  if(x == "fyke") {'FYKE'}
  else {
    if(x == 'Shock') {'SHOCK'}
  }})
    
#filter to just columns we need
IAdata1<-dplyr::select(IA_d, LAKE_CODE, GEAR2, FISH_NAME, TOTAL_COUNT) 

#group by lake and gear and species to add up all the species for a gear 
IA4 <-IAdata1 %>% group_by(LAKE_CODE, GEAR2, FISH_NAME) %>% summarise(total=sum(TOTAL_COUNT))

#make a new column called lake that is a combination of the lake code and gear type 
IA4$lake<-paste(IA4$LAKE_CODE, IA4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
IAnewformat<-tidyr::spread(IA4, FISH_NAME, total) # aggregate by lake name and gear and adds up all sp. 
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

sep<-strsplit(output_df$lake, "-")
mat<-matrix(unlist(sep), ncol=2, byrow=TRUE)
df<- as.data.frame(mat)
colnames(df) <- c("LAKE_CODE", "GEAR") 
output_IA<- cbind(df, output_df)

IA_d$LAKE_CODE<-as.factor(IA_d$LAKE_CODE)
IAdata2<-left_join(IA_d, output_IA, by = c("LAKE_CODE", "GEAR2"="GEAR"))

IAdata_lessgears<-filter(IAdata2, obs_spec > 1) #remove lake-gears with only one species 
no_plat<-filter(output_IA, slope > 0.05 & obs_spec > 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_CODE)),] # how many lakes do we lose? 35
bad_lakes<-dplyr::select(insuf_samples, LAKE_CODE) #9 lakes
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
IA_d2<-anti_join(IAdata_lessgears, bad_lakes)

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
IAdata_iNEXT<-dplyr::select(IA_d2, LAKE_CODE, FISH_NAME, TOTAL_COUNT) %>% 
  group_by(LAKE_CODE, FISH_NAME) %>% summarise(total=sum(TOTAL_COUNT)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(IAdata_iNEXT, LAKE_CODE, total) #118 lakes left and 38 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(FISH_NAME))  #dataset format to use for iNEXT code 

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
write.csv(IA_d2, 'Datasets/IA_data_QAQC.csv',row.names = FALSE)
write.csv(IA_diversity_output, 'Datasets/IA_div.csv', row.names = FALSE )

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

#filter to just columns we need
NHMEdata1<-dplyr::select(NHME_d, LAKE_ID, GEAR2, NAME_COM, TOTAL_COUNT) 

#group by lake and gear and species to add up all the species for a gear 
NHME4 <-NHMEdata1 %>% group_by(LAKE_ID, GEAR2,  NAME_COM) %>% summarise(total=sum(TOTAL_COUNT))

#make a new column called lake that is a combination of the lake code and gear type 
NHME4$lake<-paste(NHME4$LAKE_ID, NHME4$GEAR2, sep="-")  

#rearrange table to have species along the top for vegan package
NHMEnewformat<-tidyr::spread(NHME4, NAME_COM, total) # aggregate by lake name and gear and adds up all sp. 
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

sep<-strsplit(output_df$lake, "-")
mat<-matrix(unlist(sep), ncol=2, byrow=TRUE)
df<- as.data.frame(mat)
colnames(df) <- c("LAKE_ID", "GEAR") 
output_NHME<- cbind(df, output_df)

NHMEdata2<-left_join(NHME_d, output_NHME, by = c("LAKE_ID", "GEAR2"="GEAR"))

NHMEdata_lessgears<-filter(NHMEdata2, obs_spec > 1) #remove lake-gears with only one species 
no_plat<-filter(output_NHME, slope > 0.05 & obs_spec > 1) #results that do not meet the criteria 
insuf_samples<-no_plat[!duplicated(paste(no_plat$LAKE_ID)),] # how many lakes do we lose? 35
bad_lakes<-dplyr::select(insuf_samples, LAKE_ID) #5 lakes
# select the observations from the original dataset minus gears that only collected one sp. and remove waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
NHME_d2<-anti_join(NHMEdata_lessgears, bad_lakes)

#use iNEXT to estimate expected number of species and shannon diversity for every lake/reservoir
#filter to just columns we need
NHMEdata_iNEXT<-dplyr::select(NHME_d2, LAKE_ID, NAME_COM, TOTAL_COUNT) %>% 
  group_by(LAKE_ID, NAME_COM) %>% summarise(total=sum(TOTAL_COUNT)) #group by lake and sp to add up all the sp for a lake 

#change the data format so that all of the lakes are columns and species are rows. This is the iNEXT format needed.
newform<-tidyr::spread(NHMEdata_iNEXT, LAKE_ID, total) #79 lakes left and 48 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(NAME_COM))  #dataset format to use for iNEXT code 

#use iNEXT to estimate expected number of species   
#run rafaction loop from iNEXT code 
lake_names<-NHMEdata_iNEXT[!duplicated(paste(NHMEdata_iNEXT$LAKE_ID)),] # 79 unique lakes
NH_names<-filter(lake_names, grepl("NH", LAKE_ID, fixed = TRUE)) ## see how many for NH - 27
ME_names<-filter(lake_names, grepl("ME", LAKE_ID, fixed = TRUE))## see how many for ME - 52
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
write.csv(NHME_d2, 'Datasets/NHME_data_QAQC.csv',row.names = FALSE)
write.csv(NHME_diversity_output, 'Datasets/NHME_div.csv', row.names = FALSE )

res<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/reservoir data/res_class_man_all.csv", header=TRUE)


#------ Step 3: Merge fish data with LAGOS -------
# Read SHAPEFILE.shp from the LAGOS database 
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/LAGOS_NE_All_Lakes_4ha", layer = "LAGOS_NE_All_Lakes_4ha")
crs(lakes_shape) #shows me the projection of the shapefiles so that I can project the same to the points 
prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")

#project Michigan Data 
MIpoints<-dplyr::select(MIdata2, LAKE_CODE, LAT_DD, LONG_DD)
MIpoints<- MIpoints[!duplicated(paste(MIpoints$LAKE_CODE)),] #select only one row of each lake
MIpoints<-MIpoints[!(is.na(MIpoints$LAT_DD)),] #get rid of two lakes that don't have lat/long
x <- MIpoints$LONG_DD
y <- MIpoints$LAT_DD

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   
MI.aea <- spTransform(lake.ll, prjnew)
plot(MI.aea)

#project Wisconsin Data 
WI_info2$WBIC2<-as.character(WI_info2$WBIC)
WIdataLL<-left_join(WIdata2, WI_info2, by = c("LAKE_CODE" = "WBIC2")) ## join the lat lon info # lose 23 lakes that dont match lake codes
WIpoints<-dplyr::select(WIdataLL, LAKE_CODE, LAT_DD, LONG_DD)
WIpoints<- WIpoints[!duplicated(paste(WIpoints$LAKE_CODE)),] #select only one row of each lake
WIpoints<-WIpoints[!(is.na(WIpoints$LAT_DD)),] #get rid of two lakes that don't have lat/long
x <- WIpoints$LONG_DD
y <- WIpoints$LAT_DD

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
lake.ll <- SpatialPoints(coords, proj4string=prj)   
WI.aea <- spTransform(lake.ll, prjnew)
plot(WI.aea)

#Project Iowa data 
IApoints<-dplyr::select(IAdata2, LAKE_CODE, UTM_NAD83__N, UTM_NAD83__E)
IApoints<- IApoints[!duplicated(paste(IApoints$LAKE_CODE)),] #select only one row of each lake
x <- IApoints$UTM_NAD83__N
y <- IApoints$UTM_NAD83__E

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=utm +zone=15 +datum=NAD83")
lake.utm <- SpatialPoints(coords, proj4string=prj)   
IA.aea <- spTransform(lake.utm, prjnew)
plot(IA.aea)

#Project NH/ME data 
NHMEpoints<-dplyr::select(NHMEdata2, LAKE_CODE, LAT_DD, LON_DD)
NHMEpoints<- NHMEpoints[!duplicated(paste(NHMEpoints$LAKE_CODE)),] #select only one row of each lake
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
only.MI.fish<-sp::over(MI.aea, lakes_shape) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
only.MI.fish$LAKE_CODE<-paste(MIpoints$LAKE_CODE)  


#get rid of NAs (93 lakes left) 
only.MI.fish<-only.MI.fish[!(is.na(only.MI.fish$lagoslakeid)),]


#------ Step 3: add possible predictor values from LAGOS -------
#install.packages("LAGOSNE")
library(LAGOSNE)
lg <- lagosne_load("1.087.1")

#watershed scale land use/cover
IWS_LULC <- lg$iws.lulc
IWS_LULC <- data.frame(lagoslakeid=IWS_LULC$lagoslakeid,
                       iws_openwater2006_pct=IWS_LULC$iws_nlcd2006_pct_11,
                       iws_developed_open2006_pct=IWS_LULC$iws_nlcd2006_pct_21,
                       iws_developed_low2006_pct=IWS_LULC$iws_nlcd2006_pct_22,
                       iws_developed_med2006_pct=IWS_LULC$iws_nlcd2006_pct_23,
                       iws_developed_high2006_pct=IWS_LULC$iws_nlcd2006_pct_24,
                       iws_deciduous2006_pct=IWS_LULC$iws_nlcd2006_pct_41,
                       iws_evergreen2006_pct=IWS_LULC$iws_nlcd2006_pct_42,
                       iws_mixedforest2006_pct=IWS_LULC$iws_nlcd2006_pct_43,
                       iws_pasture_hay2006_pct=IWS_LULC$iws_nlcd2006_pct_81,
                       iws_rowcrops2006_pct=IWS_LULC$iws_nlcd2006_pct_82,
                       iws_roaddensity_mperha=IWS_LULC$iws_roaddensity_density_mperha,
                       iws_wetland_woody2006_pct=IWS_LULC$iws_nlcd2006_pct_90,
                       iws_wetland_emergent2006_pct=IWS_LULC$iws_nlcd2006_pct_95)

IWS_LULC$iws_nlcd2006_urb <- IWS_LULC$iws_developed_open2006_pct +
  IWS_LULC$iws_developed_low2006_pct + IWS_LULC$iws_developed_med2006_pct +
  IWS_LULC$iws_developed_high2006_pct

IWS_LULC$iws_nlcd2006_for <- IWS_LULC$iws_deciduous2006_pct +
  IWS_LULC$iws_evergreen2006_pct + IWS_LULC$iws_mixedforest2006_pct

IWS_LULC$iws_nlcd2006_agr <- IWS_LULC$iws_pasture_hay2006_pct +
  IWS_LULC$iws_rowcrops2006_pct

IWS_LULC$iws_nlcd2006_wet <- IWS_LULC$iws_wetland_woody2006_pct +
  IWS_LULC$iws_wetland_emergent2006_pct

# lake morphometry and connectivity
# max and mean depth
depth <- data.frame(lagoslakeid=lg$lakes_limno$lagoslakeid,
                    maxdepth_m=lg$lakes_limno$maxdepth,
                    meandepth_m=lg$lakes_limno$meandepth)

#lake area, perim, sdf, lat, long, elevation, and all ids
lake_morphometry <- data.frame(lagoslakeid=lg$locus$lagoslakeid,
                               HU4_ZoneID=lg$locus$hu4_zoneid,
                               HU6_ZoneID=lg$locus$hu6_zoneid,
                               HU8_ZoneID=lg$locus$hu8_zoneid,
                               HU12_ZoneID=lg$locus$hu12_zoneid,
                               nhd_lat=lg$locus$nhd_lat,
                               nhd_long=lg$locus$nhd_long,
                               lakearea_ha=lg$locus$lake_area_ha,
                               lakeperim_m=lg$locus$lake_perim_meters,
                               elevation_m=lg$locus$ elevation_m)

#calculate shoreline development: convert lake area to km2; convert lake perim to km
#lake_sdf = lake_perim_km / (2 * (pi * lake_area_km2)^1/2
lake_morphometry$lakearea_km2 <- lake_morphometry$lakearea_ha / 100
lake_morphometry$lakeperim_km <- lake_morphometry$lakeperim_m / 1000
lake_morphometry$lake_sdf     <- lake_morphometry$lakeperim_km/(2 *
                                                                  (pi*lake_morphometry$lakearea_km2)^0.5)


# iws connectivity
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        glaciation=lake_conn$latewisconsinglaciation_glacial,
                        conn_class=lake_conn$lakeconnection,
                        iws_upstream_lakes_4ha_area_ha=lake_conn$upstream_lakes_4ha_area_ha,
                        iws_wlconnections_area_ha=lake_conn$wlconnections_allwetlands_contributing_area_ha)

IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid,
                       iws_streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha)
#### HU4 variables 
#climate 
HU4_climate <- lg$hu4.chag
HU4_climate <- data.frame(HU4_ZoneID=HU4_climate$hu4_zoneid,
                          prism_precip_mean=HU4_climate$hu4_prism_ppt_30yr_normal_800mm2_annual_mean,
                          prism_temp_mean=HU4_climate$hu4_prism_tmean_30yr_normal_800mm2_annual_mean)

HU4_LULC <- lg$hu4.lulc
HU4_LULC <- data.frame(HU4_ZoneID=HU4_LULC$hu4_zoneid,
                       developed_open2006_pct=HU4_LULC$hu4_nlcd2006_pct_21,
                       developed_low2006_pct=HU4_LULC$hu4_nlcd2006_pct_22,
                       developed_med2006_pct=HU4_LULC$hu4_nlcd2006_pct_23,
                       developed_high2006_pct=HU4_LULC$hu4_nlcd2006_pct_24,
                       deciduous2006_pct=HU4_LULC$hu4_nlcd2006_pct_41,
                       evergreen2006_pct=HU4_LULC$hu4_nlcd2006_pct_42,
                       mixedforest2006_pct=HU4_LULC$hu4_nlcd2006_pct_43,
                       pasture_hay2006_pct=HU4_LULC$hu4_nlcd2006_pct_81,
                       rowcrops2006_pct=HU4_LULC$hu4_nlcd2006_pct_82,
                       hu4_roaddensity_mperha=HU4_LULC$hu4_roaddensity_density_mperha,
                       wetland_woody2006_pct=HU4_LULC$hu4_nlcd2006_pct_90,
                       wetland_emergent2006_pct=HU4_LULC$hu4_nlcd2006_pct_95)

HU4_LULC$hu4_nlcd2006_urb <- HU4_LULC$developed_open2006_pct +
  HU4_LULC$developed_low2006_pct + HU4_LULC$developed_med2006_pct +
  HU4_LULC$developed_high2006_pct

HU4_LULC$hu4_nlcd2006_for <- HU4_LULC$deciduous2006_pct +
  HU4_LULC$evergreen2006_pct + HU4_LULC$mixedforest2006_pct

HU4_LULC$hu4_nlcd2006_agr <- HU4_LULC$pasture_hay2006_pct +
  HU4_LULC$rowcrops2006_pct

HU4_LULC$hu4_nlcd2006_wet <- HU4_LULC$wetland_woody2006_pct +
  HU4_LULC$wetland_emergent2006_pct

# HU4 connectivity
HU4_conn <- lg$hu4.conn
HU4_conn <- data.frame(HU4_ZoneID=HU4_conn$hu4_zoneid,
                       hu4_streamdensity_mperha=HU4_conn$hu4_streamdensity_streams_density_mperha,
                       hu4_lakes_area_ha=HU4_conn$hu4_lakes_overlapping_area_ha,
                       hu4_wetlands_area_ha=HU4_conn$hu4_wl_allwetlandsdissolved_overlapping_area_ha)


#merge all local predictors
local_preds <- left_join(lake_morphometry, depth, by = "lagoslakeid") %>%
  left_join(IWS_LULC, by = "lagoslakeid") %>%
  left_join(lake_conn, by = "lagoslakeid") %>%
  left_join(IWS_conn, by = "lagoslakeid")

#merge all HU4 predictors 
hu4_preds <- left_join(HU4_climate, HU4_conn, by = "HU4_ZoneID") %>%
  left_join(HU4_LULC, by = "HU4_ZoneID") 

## Merge LAGOS to MI lakes 
local_preds$lagoslakeid<-as.factor(local_preds$lagoslakeid)
fish.local<-left_join(fish.LAGOS, local_preds, by = 'lagoslakeid')

MI.fish.LAGOS<-left_join(fish.local, hu4_preds, by = 'HU4_ZoneID')

#plot to make sure join was correct
ggplot() + 
  geom_point(data=MI.fish.LAGOS, aes(x=nhd_long, y=nhd_lat))

#add reservoir and natural lake data 
#lakes<- read.csv("/Users/katelynking/Desktop/mi_nl_ll.csv", header=TRUE)

#newfish<-dplyr::left_join(MI.fish.LAGOS, lakes, by= "lagoslakeid")


##pull out only columns for initial model development 
#fish.data<-dplyr::select(MI.fish.LAGOS, lakearea_km2, elevation_m, LakeConnec, lagoslakei, 
 #                        HU4_ZoneID, richness, nhd_lat, nhd_long, lake_sdf, 
  #                       iws_nlcd2006_wet, iws_nlcd2006_agr, iws_nlcd2006_for, iws_nlcd2006_urb, iws_roaddensity_mperha,
   #                      iws_upstream_lakes_4ha_area_ha, iws_wlconnections_area_ha, iws_streamdensity_mperha,
    #                     prism_precip_mean, prism_temp_mean, 
     #                    hu4_streamdensity_mperha,hu4_lakes_area_ha,hu4_wetlands_area_ha, 
      #                   hu4_nlcd2006_urb, hu4_nlcd2006_for, hu4_nlcd2006_agr, hu4_nlcd2006_wet, hu4_roaddensity_mperha)

write.csv(MI.fish.LAGOS, "Data/MI_lake_data.csv", row.names = FALSE)

#fish.data<-read.csv('Data/MI_lake_data.csv')

#------ Step 4: calculate beta diversity for HUC4s
library(vegan)
#filter to just columns we need
MIdata1<-dplyr::select(MIdata, LAKE_CODE, SPP_CODE, Total_Number_Caught) 

#group by lake and species to add up all the species for a lake
MIdata2 <-MIdata1 %>% group_by(LAKE_CODE, SPP_CODE) %>% summarise(total=sum(Total_Number_Caught))
MIdata3<-tidyr::spread(MIdata2, SPP_CODE, total)

HUC_codes<-dplyr::select(MI.fish.LAGOS, LAKE_CODE, lagoslakeid, HU4_ZoneID, HU6_ZoneID)
MIdata4<-dplyr::left_join(MIdata3, HUC_codes)
MIdata4[is.na(MIdata4)] <- 0 #NAs to 0 
MIdata4<-MIdata4[!(is.na(MIdata4$HU6_ZoneID)),] #get rid of 5 lakes that didn't match to LAGOS

### create a function to calculate beta diversity average for a region
 calcbeta<-function(x) { # x = a dataframe   
   new<- subset(x, select = -c(LAKE_CODE, lagoslakeid, HU4_ZoneID, HU12_ZoneID))
   #dissimilarity
   dis<-vegdist(new, "bray", binary = TRUE) 
   #average dissimilarity
   avg<-mean(dis)
   return(avg)
 }
 
# loop to run dissimilarity on all HUC 4s
HUC_string <-as.character(unique(MIdata4$HU4_ZoneID))

output_df <- data.frame(HU4_ZoneID=NA, beta=NA)            
for (i in 1:8) {
  sub<-subset(MIdata4, HU4_ZoneID == HUC_string[i] )
  beta<-calcbeta(sub)
  output_df[i,1]=HUC_string[i]
  output_df[i,2]=beta
} 

### loop to run on all HUC12s 
HUC_string <-as.character(unique(MIdata4$HU12_ZoneID))

output_df <- data.frame(HU6_ZoneID=NA, beta=NA)            
for (i in 1:10) {
  sub<-subset(MIdata4, HU6_ZoneID == HUC_string[i] )
  beta<-calcbeta(sub)
  output_df[i,1]=HUC_string[i]
  output_df[i,2]=beta
} 


#join the beta outputs 
MI.fish_beta <-left_join(MI.fish.LAGOS, output_df, by = 'HU6_ZoneID')

## compare alpha and beta diversity (Pool et al 2014)
ggplot(data = MI.fish_beta, (aes(x = richness, y = beta ))) + geom_point() +
  geom_smooth(method='lm') 

## bar graph of the disimilarity values within each HUC to compare distribution (left skewed in the Roden 2018 paper)

## making maps 
#base map of michigan 
usa<-map_data("usa")  #pull out the usa map
states<-map_data("state")  #pull out the states maps 
MImap <- subset(states, region %in% c("michigan")) # and within that select michigan 
p<-ggplot(data = MImap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

#map of beta diversity at the HUC4 scale 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
   labs(color="beta diversity")

#map of beta diversity at the HUC6 scale 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(beta))) + scale_colour_gradient(low = "yellow", high = "darkblue") + 
  labs(color="beta diversity")

#map of alpha diversity 
p + 
  geom_point(data=MI.fish_beta, aes(x=nhd_long, y=nhd_lat, colour = c(richness))) + scale_colour_gradient(low = "yellow", high = "darkblue") +
  labs(color="alpha diversity")

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


#------ Step 5: standardize and transform predictor values from LAGOS -------

