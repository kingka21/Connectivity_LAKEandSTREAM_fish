#### Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Oct 15, 2018
## Updated: Aug 2019

#load libraries
library(rgdal)
library(sp)
library(dplyr)
library(raster)

#------ Step 1: QA/QC fish data -------
stream_data<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/Fish streams/stream_fish_5_states.csv", header = TRUE)

#get rid of hybrid and unknown species - no hybrids or unknowns present 
stream_species<-dplyr::select(stream_data, COMIDv2_JR, common_name, scientific_name) 

#select years to match the lakes 
ME_data<-filter(stream_data, STATE_AB =="ME")
MI_data<-filter(stream_data, STATE_AB =="MI")
IA_data<-filter(stream_data, STATE_AB =="IA")
NH_data<-filter(stream_data, STATE_AB =="NH")
WI_data<-filter(stream_data, STATE_AB =="WI")

MI_d<-filter(MI_data, samp_year == "2003" | samp_year == "2004" | samp_year == "2005"| samp_year == "2006")
WI_d<-filter(WI_data, samp_year == "2001" | samp_year == "2002" | samp_year == "2003"| samp_year == "2004" | samp_year == "2005")
IA_d<-filter(IA_data, samp_year == "2001" | samp_year == "2002" | samp_year == "2003"| samp_year == "2004")
ME_d<-filter(ME_data, samp_year == "2006" | samp_year == "2007" | samp_year == "2008"| samp_year == "2009" | samp_year == "2010" | samp_year == "2011")
NH_d<-filter(NH_data, samp_year < 2011 ) 

#bind everything back into one table 
stream_d<-gtools::smartbind(MI_d, WI_d, IA_d, ME_d, NH_d)

#clean up species names for cross ecosystems comparisons 
stream_d$SPP_CODE_CAP<-casefold(stream_d$common_name, upper = TRUE) #make them all cap
stream_d$SPP_CODE_n<-gsub(" ", "_", stream_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space
stream_d$SPP_CODE_new<- ifelse(stream_d$SPP_CODE_n == 'TROUT-PERCH', 'TROUT_PERCH', stream_d$SPP_CODE_n) 

allsp<-stream_d[!duplicated(paste(stream_d$SPP_CODE_new)),] #159 


#------ Step 2: rarefaction -------
#filter to just columns we need
streamdata1<-dplyr::select(stream_d, COMIDv2_JR, SPP_CODE_new, ind_count) 

#group by stream and species 
stream2 <-streamdata1 %>% group_by(COMIDv2_JR, SPP_CODE_new) %>% summarise(total=sum(ind_count))

#rearrange table to have species along the top for vegan package
stream_vegformat<-tidyr::spread(stream2, SPP_CODE_new, total) # aggregate by stream name
stream_vegformat[is.na(stream_vegformat)] <- 0 #NAs to 0 

st_vegformat<-subset(stream_vegformat, select = -c(COMIDv2_JR) )   #get rid of columns I do not want

## run rarefaction via Vegan code 
### first create an empty data frame to save all of the output 
output_df <-data.frame(matrix(NA, nrow = length(stream_vegformat$COMIDv2_JR), ncol = 3))
output_df[,1] = stream_vegformat$COMIDv2_JR #put all the lake names in the first column
colnames(output_df) <- c("COMIDv2_JR","obs_spec", "slope")

for (i in 1:nrow(st_vegformat)) { #this runs all rows
  sub<-st_vegformat[i,] #takes each row
  obs<-specnumber(sub) #richness (observed)
  sample<- rowSums(sub) #sums all samples so you have the total number of individuals caught 
  slope <- rareslope(sub, sample=(sample*.9))  #90% of total indv. caught, slope we keep if <0.05    
  out3 <-round(slope, 3)
  output_df[i,2]<-obs #puts observed in the table 
  output_df[i,3]<-out3  ##this puts slope in the table 
}

STdata2<-left_join(stream_d, output_df)
no_plat<-filter(output_df, slope > 0.05 | obs_spec == 1) #results that do not meet the criteria  #1050
# select the observations from the original dataset minus waterbodies that do not plateau 
#anti_join returns all rows from data that do not match bad_lakes)
stream_d2<-anti_join(STdata2, no_plat, by='COMIDv2_JR')

#use iNEXT to estimate expected number of species and shannon diversity for every stream reach 
#filter to just columns we need
stream_iNEXT<-dplyr::select(stream_d2, COMIDv2_JR, SPP_CODE_new, ind_count) %>% 
  group_by(COMIDv2_JR, SPP_CODE_new) %>% summarise(total=sum(ind_count)) #group by reach and sp 

#change the data format to iNEXT format needed.
newform<-tidyr::spread(stream_iNEXT, COMIDv2_JR, total) #1984 reaches left and 155 species

#NAs to 0 
newform[is.na(newform)] <- 0 

# format has to be as a data frame for input into iNEXT
newform<-as.data.frame(newform)

#remove species codes 
new<-subset(newform, select = -c(SPP_CODE_new))  #dataset format to use for iNEXT code 

#run rafaction loop from iNEXT code 
stream_names<-stream_iNEXT[!duplicated(paste(stream_iNEXT$COMIDv2_JR)),] # 1984 unique 
stream_diversity_output <-data.frame(matrix(NA, nrow = length(stream_names$COMIDv2_JR), ncol = 3))
stream_diversity_output[,1] = stream_names$COMIDv2_JR #put all the names in the first column
colnames(stream_diversity_output) <- c("COMIDv2_JR","exp_rich", "shannon")
for (i in 1:ncol(new)) {  #this runs all columns 
  out <- iNEXT(new[,i], q=0, datatype="abundance")    
  expected <-out$AsyEst[1,2]   #### select the species richness estimate 
  shannon <-out$AsyEst[2,2]  #### select the Shannon diversity estimate
  stream_diversity_output[i,2]<-expected ##this puts expected in the table  
  stream_diversity_output[i,3]<-shannon # put shannon value in the table 
}

## save output of clean datasets 
write.csv(stream_d2, 'Datasets/stream_data_QAQC.csv',row.names = FALSE)
write.csv(stream_diversity_output, 'Datasets/stream_div.csv', row.names = FALSE )

#state summary
uniq_reaches<-stream_d2[!duplicated(paste(stream_d2$COMIDv2_JR)),] 
summary(uniq_reaches$STATE_AB)

#------ Step 3: join data with HUC4 and LAGOS -------
stream_div<-read.csv('Datasets/stream_div.csv')
stream_dat<-read.csv('Datasets/stream_data_QAQC.csv')

streampoints<-dplyr::select(stream_dat, COMIDv2_JR, lat, lon)
streampoints<- streampoints[!duplicated(paste(streampoints$COMIDv2_JR)),] #select only one row of each stream
x <- streampoints$lon
y <- streampoints$lat

#make a dataframe of the coordinates, project, transform
coords<-data.frame(x=x,y=y)
prj <- CRS("+proj=longlat +datum=NAD83")
stream.ll <- SpatialPoints(coords, proj4string=prj)   

prjnew <- CRS(" +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
              +ellps=GRS80 +towgs84=0,0,0")
stream.aea <- spTransform(stream.ll, prjnew)
plot(stream.aea)

#use the over function to match points to LAGOS HUC4s, HUC8, and HUC12. 
HU4.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU4", layer = "HU4")
HU8.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU8", layer = "HU8")
HU12.poly<-readOGR(dsn = "/Users/katelynking/Desktop/HU12", layer = "HU12")
names(HU4.poly)
crs(HU4.poly) #shows me the projection of the shapefiles so that I can project the same to the points 
crs(stream.aea) <- crs(HU4.poly) # points to the same projection

#Returns a data.frame of the second argument with row entries corresponding to the first argument
streams_HU4<-sp::over(stream.aea, HU4.poly) #x = "SpatialPoints", y = "SpatialPolygonsDataFrame"
streams_HU4$COMIDv2_JR<-paste(streampoints$COMIDv2_JR)  
streams.matchHU4<-dplyr::select(streams_HU4, COMIDv2_JR, ZoneID)
streams.matchHU4$COMIDv2_JR<-as.integer(streams.matchHU4$COMIDv2_JR)

streams_HU8<-sp::over(stream.aea, HU8.poly)
streams_HU8$COMIDv2_JR<-paste(streampoints$COMIDv2_JR)  
streams.matchHU8<-dplyr::select(streams_HU8, COMIDv2_JR, ZoneID)
streams.matchHU8$COMIDv2_JR<-as.integer(streams.matchHU8$COMIDv2_JR)

streams_HU12<-sp::over(stream.aea, HU12.poly) 
streams_HU12$COMIDv2_JR<-paste(streampoints$COMIDv2_JR)  
streams.matchHU12<-dplyr::select(streams_HU12, COMIDv2_JR, ZoneID)
streams.matchHU12$COMIDv2_JR<-as.integer(streams.matchHU12$COMIDv2_JR)

##add HUC IDs to the streams data 
stream.merge.HU4<-dplyr::left_join(stream_dat, streams.matchHU4) %>%
  left_join(streams.matchHU8, by="COMIDv2_JR") %>%
  left_join(streams.matchHU12, by="COMIDv2_JR") %>%
  left_join(stream_div)

streamsall<-stream.merge.HU4[!(is.na(stream.merge.HU4$ZoneID)),]  # remove streams that did not match to HU4 (only 3!) 
data.table::setnames(streamsall, old=c("ZoneID.x","ZoneID.y", 'ZoneID'), new=c("HU4_ZoneID", "HU8_ZoneID", "HU12_ZoneID"))

write.csv(streamsall, "Datasets/streamsall.csv", row.names = FALSE)
