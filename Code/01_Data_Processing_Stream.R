#### Fish diversity across lakes and streams at the regional scale
## Written by Katelyn King Oct 15, 2018
## Updated: Nov 2019

#load libraries
library(rgdal)
library(sp)
library(sf)
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
ME_d<-filter(ME_data, samp_year == "2006" | samp_year == "2007" | samp_year == "2008"| samp_year == "2009")
NH_d<-filter(NH_data, samp_year == "1994" | samp_year == "1999" | samp_year == "2000" | samp_year == "2001" ) 

#bind everything back into one table 
stream_d<-gtools::smartbind(MI_d, WI_d, IA_d, ME_d, NH_d)

#clean up species names for cross ecosystems comparisons 
stream_d$SPP_CODE_CAP<-casefold(stream_d$common_name, upper = TRUE) #make them all cap
stream_d$SPP_CODE_n<-gsub(" ", "_", stream_d$SPP_CODE_CAP, fixed=TRUE) #add a dash where there is a space
stream_d$SPP_CODE_new<- ifelse(stream_d$SPP_CODE_n == 'TROUT-PERCH', 'TROUT_PERCH', stream_d$SPP_CODE_n) 
stream_d$SPP_CODE_new<- ifelse(stream_d$SPP_CODE_n == 'HORNEYHEAD_CHUB', 'HORNYHEAD_CHUB', stream_d$SPP_CODE_n) 

allsp<-stream_d[!duplicated(paste(stream_d$SPP_CODE_new)),] #155 

#------ Step 2: rarefaction -------
library(vegan)
library(iNEXT)

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
stream_names<-stream_iNEXT[!duplicated(paste(stream_iNEXT$COMIDv2_JR)),] # 1572 unique 
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

##clean datasets with necessary columns
stream_data_QAQC<-dplyr::select(stream_d2, COMIDv2_JR, lat, lon, samp_year, samp_length, width, common_name, scientific_name, ind_count, STATE_AB, SPP_CODE_new, obs_spec, slope) %>%
                          left_join(stream_diversity_output)

#state summary
uniq_reaches<-stream_d2[!duplicated(paste(stream_d2$COMIDv2_JR)),] 
summary(uniq_reaches$STATE_AB)

#------ Step 3: join data with NHD info, HUCs, and LAGOS -------
stream_dat<-stream_data_QAQC

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
streams.matchHU4<-dplyr::select(streams_HU4, COMIDv2_JR, ZoneID, HUC4) #HUC4 is the USGS name, ZoneID is a name given by LAGOS
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
  left_join(streams.matchHU12, by="COMIDv2_JR") 
 

streamsall<-stream.merge.HU4[!(is.na(stream.merge.HU4$ZoneID)),]  # remove streams that did not match to HU4 (only 3!) 
data.table::setnames(streamsall, old=c("lat", "lon", "STATE_AB", "ZoneID.x","ZoneID.y", 'ZoneID'), new=c("LAT_DD", "LON_DD", "STATE", "HU4_ZoneID", "HU8_ZoneID", "HU12_ZoneID"))

#merge with Stream Order info! 

### Use Joe's package for the NHDPlus data 
# install.packages("devtools")
devtools::install_github("jsta/nhdR")
library(nhdR)
## need VPUs (Vector Processing Units) 1, 4, 7, 10 
# get a vpu export
nhd_plus_get(vpu = 1, "NHDPlusAttributes")
nhd_plus_get(vpu = 1, "NHDSnapshot") ## snapshot has area data
nhd_plus_list(vpu = 1, "NHDSnapshot")
nhd_plus_list(vpu = 1, "NHDPlusAttributes")
st_order01<-nhd_plus_load(vpu = 1, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde) #has stream order
st_area01<-nhd_plus_load(vpu = 1, "NHDSnapshot", "NHDFlowline") %>% 
  dplyr::select(COMID, LENGTHKM)  %>%  #reach length 
  st_drop_geometry() #library(sf)

nhd_plus_get(vpu = 4, "NHDPlusAttributes")
nhd_plus_get(vpu = 4, "NHDSnapshot")
st_order04<-nhd_plus_load(vpu = 4, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde) #has stream order
st_area04<-nhd_plus_load(vpu = 4, "NHDSnapshot", "NHDArea") %>% 
  dplyr::select(COMID, AREASQKM)  %>%  #stream area
  st_drop_geometry() #library(sf)

nhd_plus_get(vpu = 7, "NHDPlusAttributes")
nhd_plus_get(vpu = 7, "NHDSnapshot")
st_order07<-nhd_plus_load(vpu = 7, "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde) #has stream order
st_area07<-nhd_plus_load(vpu = 7, "NHDSnapshot", "NHDArea") %>% 
  dplyr::select(COMID, AREASQKM)  %>%  #stream area
  st_drop_geometry() #library(sf)

nhd_plus_get(vpu = '10U', "NHDPlusAttributes")
nhd_plus_get(vpu = '10U', "NHDSnapshot")
st_order10U<-nhd_plus_load('10U', "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde) #has stream order
st_area10U<-nhd_plus_load(vpu = '10U', "NHDSnapshot", "NHDArea") %>% 
  dplyr::select(COMID, AREASQKM)  %>%  #stream area
  st_drop_geometry() #library(sf)

nhd_plus_get(vpu = '10L', "NHDPlusAttributes")
nhd_plus_get(vpu = '10L', "NHDSnapshot")
st_order10L<-nhd_plus_load(vpu = '10L', "NHDPlusAttributes", "PlusFlowlineVAA") %>% 
  dplyr::select(ComID, StreamOrde) #has stream order
st_area10L<-nhd_plus_load(vpu = '10L', "NHDSnapshot", "NHDArea") %>% 
  dplyr::select(COMID, AREASQKM)  %>%  #stream area
  st_drop_geometry() #library(sf)

#read in saved csv files 
st_order01<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/FlowlineVAA1.csv", header = TRUE) %>%
  dplyr::select(ComID, StreamOrde) %>%
  rename(COMID = ComID)
st_area01<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/Flowlineinfo1.csv", header = TRUE) %>%
  dplyr::select(COMID, LENGTHKM)
st_order04<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/FlowlineVAA4.csv", header = TRUE) %>%
  dplyr::select(ComID, StreamOrde) %>%
  rename(COMID = ComID)
st_area04<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/Flowlineinfo4.csv", header = TRUE) %>%
  dplyr::select(COMID, LENGTHKM)
st_order07<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/FlowlineVAA7.csv", header = TRUE) %>%
  dplyr::select(ComID, StreamOrde) %>%
  rename(COMID = ComID)
st_area07<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/Flowlineinfo7.csv", header = TRUE) %>%
  dplyr::select(COMID, LENGTHKM)
st_order10U<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/FlowlineVAA10U.csv", header = TRUE) %>%
  dplyr::select(ComID, StreamOrde) %>%
  rename(COMID = ComID)
st_area10U<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/Flowlineinfo10U.csv", header = TRUE) %>%
  dplyr::select(COMID, LENGTHKM)
st_order10L<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/FlowlineVAA10L.csv", header = TRUE) %>%
  dplyr::select(ComID, StreamOrde) %>%
  rename(COMID = ComID)
st_area10L<-read.csv("/Users/katelynking/Desktop/NHDmed_flowlines_all/Flowlineinfo10L.csv", header = TRUE) %>%
  dplyr::select(COMID, LENGTHKM)

#combine all order data of the HUCS
HUC_order<-gtools::smartbind(st_order01, st_order04, st_order07, st_order10U, st_order10L) 

#combine all area data of the HUCS
HUC_area<-gtools::smartbind(st_area01, st_area04, st_area07, st_area10U, st_area10L) 

# pull out unique reaches and join 
stream_uniq<-streamsall[!duplicated(paste(streamsall$COMIDv2_JR)),] #1571 
stream_order<-left_join(stream_uniq, HUC_order, by = c("COMIDv2_JR" = "COMID") ) %>%
                left_join(HUC_area, by = c("COMIDv2_JR" = "COMID")) %>%
                dplyr::select(COMIDv2_JR, StreamOrde, LENGTHKM)

stream_order_info<-stream_order[!(is.na(stream_order$StreamOrde)),] # only 4 streams without order
stream_order_info$CLASS<-sapply(stream_order_info$StreamOrde, function(x) {
  if(x == '1') {'HW_stream'}
  else {
    if(x == '2') {'HW_stream'}
    else {
      if(x == '3') {'MID_stream'}
      else {
        if(x== '4') {'MID_stream'}
        else {
          if(x== '5') {'MID_stream'}
          else {
            if(x== '6') {'RIVER'}
            else {
              if(x== '7') {'RIVER'}
              else {
                if(x== '8') {'RIVER'}
              }}}}}}}})

barchart(stream_order_info$CLASS)
stream_order_info$CLASS<-as.factor(stream_order_info$CLASS)

stream_new<-left_join(streamsall, stream_order_info)

write.csv(stream_new, "Datasets/stream_data_QAQC.csv", row.names = FALSE)


#------ Step 4: sub-setting independent streams -------
#pull out the stream with the most observations of species from HUC 12s so that I don't have streams from the same watershed 
stream_w_order<-stream_new[!(is.na(stream_new$StreamOrde)),]
uniq_reaches<-stream_w_order[!duplicated(paste(stream_w_order$COMIDv2_JR)),] 
streams_sub<-do.call(rbind, lapply(split(uniq_reaches, uniq_reaches$HU12_ZoneID), function(x) {return(x[which.max(x$obs_spec),])})) 
stream_sub_ids<-dplyr::select(streams_sub, COMIDv2_JR)
streams_subset<-left_join(stream_sub_ids, stream_new)

#854 subset of streams 
write.csv(streams_subset, "Datasets/streams_subset.csv", row.names = FALSE)
streams_subset<-read.csv("Datasets/streams_subset.csv", header=TRUE)

#add in area 
HW_width<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/Fish streams/stream_data_subsets/HW_streams.csv", header = TRUE) %>%
  dplyr::select(COMIDv2_JR, width_m)
midriv_width<-read.csv("/Users/katelynking/Desktop/Chap 2 Fish/Fish streams/stream_data_subsets/mid_riv.csv", header = TRUE) %>%
  dplyr::select(COMIDv2_JR, width_m)

streamwidth<-gtools::smartbind(HW_width, midriv_width)
stream_w_widths<-left_join(streams_subset, streamwidth)

stream_w_widths$area<-(stream_w_widths$width_m*.001)*(stream_w_widths$LENGTHKM) #convert to km and then multiply

write.csv(stream_w_widths, 'Datasets/stream_w_area.csv', row.names = FALSE )

streams_subset<-stream_w_widths

#streams<-left_join(streams_subset, stream_width)
#HW_stream<-streams[!duplicated(paste(streams$COMIDv2_JR)),]%>%
 # filter(CLASS == "HW_stream")
#HW_stream <- HW_stream[!(is.na(HW_stream$width)),]
#write.csv(HW_stream, "/Users/katelynking/Desktop/HW_streams.csv", row.names = FALSE)

#big_stream<-streams[!duplicated(paste(streams$COMIDv2_JR)),]%>%
 # filter(CLASS == "MID_stream" | CLASS == "RIVER")
#write.csv(big_stream, "/Users/katelynking/Desktop/bigger_streams.csv", row.names = FALSE)


