##################################################################################
# 
# WGBEAM Tor a, b (1): Evaluate spatial-temporal distribution from combined offshore and inshore data for    elasmobranchs (e.g. Raja family)
#
# 
# Original script created by Loes, Gudjon, Giuseppe & Vaishav (25/03/2020; updated on 26/03/2021 by Loes)
# Modified by Francesco Masnadi (11/05/2022)
# 
#
# 
#################################################################################
# clear environment
rm(list=ls()) 
#test
# load libraries
library(icesDatras)
library(tidyverse)
library(dplyr)
library(tidyr)
library(worrms)

##########################################
##########################################
## Get data from DATRAS
##########################################
##########################################
## Get station (HH) data from Datras 
getSurveyList() # gives overview of all survey acronyms

tmpDYFS<-getDATRAS(record = "HH",
               survey = "DYFS",   #only 1 survey at a time
               years= 2000:2023,
               quarters = 1:4) 
# look what's there
head(tmpDYFS)

table(tmpDYFS$Country, tmpDYFS$Year)

tmpBTS<-getDATRAS(record = "HH",
                   survey = "BTS",   #only 1 survey at a time
                   years= 2000:2023,
                   quarters = 1:4) 
# look what's there
head(tmpBTS)

table(tmpBTS$Country, tmpBTS$Year)

tmpSNS<-getDATRAS(record = "HH",
                  survey = "SNS",   #only 1 survey at a time
                  years= 2000:2023,
                  quarters = 1:4) 

head(tmpSNS)

table(tmpSNS$Country, tmpSNS$Year)

# select useful colums DYFS
tmp1<-tmpDYFS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                         DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                     Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                     SurTemp, BotTemp, SurSal, BotSal)
# select useful colums BTS
tmp2<-tmpBTS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                         DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                         Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                         SurTemp, BotTemp, SurSal, BotSal)

# select useful colums SNS
tmp3<-tmpSNS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                        DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                        Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                        SurTemp, BotTemp, SurSal, BotSal)

###########
## Joining DYFS and BTS and SNS
###########
HHFIN <- rbind(tmp1,tmp2,tmp3)
# look for NAs
for (Var in names(HHFIN)) {
  missing <- sum(is.na(HHFIN[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}


######
## Get length (HL) data from Datras
# DYFS
tstart<-Sys.time() 
tmpfDYFS1 <-getDATRAS(record = "HL",
                survey = "DYFS",   #only 1 survey at a time
                years= 2000:2023,
                quarters = 1:4)  
tfinish<-Sys.time() 
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfDYFS1)
table(tmpfDYFS1$Country, tmpfDYFS1$Year)

## BTS
tmpfBTS <-getDATRAS(record = "HL",
                     survey = "BTS",   #only 1 survey at a time
                     years= 2000:2023,
                     quarters = 1:4) # time to download 10:42 to 11:47




# look what's there
head(tmpfBTS)
table(tmpfBTS$Country, tmpfBTS$Year)

## SNS
tstart<-Sys.time() 
tmpfSNS <-getDATRAS(record = "HL",
                    survey = "SNS",   #only 1 survey at a time
                    years= 2000:2023,
                    quarters = 1:4) # time to download 3 minutes
tfinish<-Sys.time() 
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfSNS)
table(tmpfSNS$Country, tmpfSNS$Year)

# select useful colums DYFS
tmpf1<-tmpfDYFS1 %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                       SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                       SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, Valid_Aphia)
# select useful colums BTS
tmpf2<-tmpfBTS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                       SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                       SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, Valid_Aphia)
# select useful colums SNS
tmpf3<-tmpfSNS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                          SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                          SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, Valid_Aphia)

###########
## Joining DYFS and BTS and SNS
###########
HLFIN <- rbind(tmpf1,tmpf2,tmpf3)
# look for NAs
for (Var in names(HLFIN)) {
  missing <- sum(is.na(HLFIN[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}



##########################################
##########################################
## Create one dataset
##########################################
##########################################
## Joining HH en HL datasets 
dat <-HLFIN %>%
  left_join(HHFIN) 

# look for NAs
for (Var in names(dat)) {
  missing <- sum(is.na(dat[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

## Create a species list from the aphia codes
sp <-dat%>%
  distinct(Valid_Aphia)
sp$scientificname <- "NA"

for(i in 1:length(sp$Valid_Aphia)){
  sp$scientificname[i] <- worrms::wm_id2name(sp$Valid_Aphia[i])
}

# ## Tidy up the species list
# sp1<-sp %>% select(Valid_Aphia,scientificname)
#
## Get species names from worms
dat1 <- dat %>% 
  left_join(sp1,by="Valid_Aphia")

# look for NAs
for (Var in names(dat1)) {
  missing <- sum(is.na(dat1[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# only take data from valid hauls
dat2 <- dat1 %>%
  filter(HaulVal == "V")


## Get width of gear from gear codes
# look which gearcodes are in the data
table(dat2$Gear)

# crate new column beamwidth 
# Beam width is the number stated in Gear, so we extract that number into a new variable #Beam_width

#option 1
#dat2$beamwidth <- substr(dat2$Gear, start = 3, stop = 3)

#option 2
dat2 <- dat2 %>%
  mutate(beamwidth = case_when(Gear == "BT8" ~ 8,
                               Gear == "BT7" ~ 7,
                               Gear == "BT6" ~ 6,
                               Gear == "BT4A" ~ 4,
                               Gear == "BT4AI" ~ 4,
                               Gear == "BT4S" ~ 4,
                               Gear == "BT3" ~ 3,
                               Gear == "BT4P" ~ 4,
                               TRUE ~ NA_real_))

# check for NAs
table(dat2$beamwidth, useNA = "always")

# set all lenght measurements in mm
table(dat2$LngtCode)
dat2 <- dat2 %>% 
  mutate(Length = ifelse (LngtCode %in% c("1", "5"), LngtClass*10, LngtClass))

# rename dataset to dat
dat <- dat2  

#save dat 
#setwd("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/WGBEAM/wg_WGBEAM/elasmobranchs")
save(dat, file = "dat2000_II.rda")


 
##########################################
##########################################
## Calculate swept area
##########################################
##########################################

# Option 1: based on beam width and distance
# check if there are missing records for Distance
load("dat2000_II.rda")
test <- dat %>%  filter(is.na(Distance))
table(test$Country, test$Year)

# When there is no distance recorded:
# Option 2: calculate swept area based on beam width, ground speed and haulduration
table(dat$GroundSpeed) 
dat$GroundSpeed[dat$GroundSpeed == -9]<- 3.5 ## Set speed to average 3.5 knots when not recorded
# create column swept area
dat<-dat %>% 
  mutate(sweptarea2 = ((beamwidth/1000)*(1.852*GroundSpeed*HaulDur/60))) ## Calculate swept area in km2


# make a list of the fished stations in the survey
stations <- dat %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, sweptarea2) %>%
  distinct()
##########################################
##########################################
## Select species ##
##########################################
##########################################

#### select only elasmobranchs from dat and merge the two mustelus species###
datspec<-dat %>%
  mutate(scientificname = if_else(scientificname=="Mustelus asterias", "Mustelus", scientificname))%>%mutate(scientificname = if_else(scientificname=="Mustelus mustelus", "Mustelus", scientificname))%>%
  mutate(Valid_Aphia = if_else(Valid_Aphia==105821, 105732, Valid_Aphia))%>%
  mutate(Valid_Aphia = if_else(Valid_Aphia==105822, 105732, Valid_Aphia)) %>%
  filter(Valid_Aphia %in% c(105883,105891,367297,105885,105887,105865,105876,105885,105820,105732,105814,105815,105923))

#### create objects for rays and sharks for plots #####
rays<-c("Amblyraja radiata","Leucoraja naevus","Raja brachyura","Raja clavata","Raja microocellata", "Raja montagui","Raja undulata")
sharks<-c("Galeorhinus galeus","Mustelus", "Scyliorhinus canicula","Scyliorhinus stellaris", "Squalus acanthias")

# calculate number of speciments that were measured
number_measured <- datspec %>% 
  select(Country, Ship, Survey, Gear, Year, HaulNo, ShootLat, ShootLong, scientificname,NoMeas) %>%
  distinct() 

number_measured <- number_measured %>% 
  group_by(Country, Ship, Survey, Gear,Year,scientificname) %>% dplyr::summarize(NumMeas = sum(NoMeas))

# put = instead of NA
number_measured[is.na(number_measured)] <- 0

##### Plot total number along the time series by country. NOT Required in the report ####
jpeg(file=paste("rajas_elasmobranch_by_source.jpeg",sep=""),width = 200, height = 200, units = "mm", res = 400)
ggplot(number_measured %>% filter(Year %in% c(2000:2023), scientificname %in% rays))+
  geom_bar(aes(x=Year, y=NumMeas, fill=Country), stat = "identity", position="dodge") +
  facet_wrap(~scientificname, scales="free_y")
dev.off() 


# Calculate numbers per station
SpecNo <- datspec %>% 
  group_by(Country, Ship, Survey, Gear, Year, HaulNo, ShootLat, ShootLong, SubFactor, SweptArea_km2, scientificname,TotalNo) %>% 
  #filter(HLNoAtLngt>0 & SubFactor>0) %>% 
  dplyr::summarize(No = sum(HLNoAtLngt)) %>% 
  dplyr::mutate(TotalNo1 = ifelse(TotalNo>0, TotalNo,  #if TotalNo is filled in than take this value
                           ifelse(No>0 & SubFactor>0, No*SubFactor, NA))) # when TotalNo not filled in, calculate TotalNo based on HLNoAtLngt and SubFactor

##### Plot total number along the time series by country. Required in the report ####
jpeg(file=paste("rajas_elasmobranch.jpeg",sep=""),width = 200, height = 200, units = "mm", res = 400)
ggplot(SpecNo %>% filter(scientificname %in% rays))+
  geom_bar(aes(x=Year, y=TotalNo1, fill=Country),stat = "identity")+ylab("Total number")+
  facet_wrap(~scientificname, scales = "free_y")+ theme(legend.position=c(0.4,0.2))
dev.off()

jpeg(file=paste("sharks_elasmobranch_totN.jpeg",sep=""),width = 200, height = 200, units = "mm", res = 400)
ggplot(SpecNo %>% filter(scientificname %in% sharks))+
  geom_bar(aes(x=Year, y=TotalNo1, fill=Country),stat = "identity")+ylab("Total number")+
  facet_wrap(~scientificname, scales = "free_y")+ theme(legend.position=c(0.8,0.2))
dev.off()

##########################################
##########################################
## CPUE
##########################################
##########################################
# Calculate CPUE (numbers/km2) by haul
byhaul <- SpecNo %>% 
  mutate(fishkm2 = TotalNo1/sweptarea2) %>% 
  mutate(sqrt_fishkm2 = sqrt(fishkm2))

### Plot catches
## Map by Haul ##

#first, make map of the north sea #
library(mapdata)
m <-map_data("worldHires") 

## set map boundaries
xmin <- min(stations$ShootLong)
xmax <- max(stations$ShootLong)
ymin <- 48.5    # using min(stations$ShootLat) there is a wrong Lat data in 2012
ymax <- max(stations$ShootLat)

###################
### Map by Year ##
###################
# jpeg(file=paste("Sharks_map_by_year.jpeg",sep=""),width = 250, height = 200, units = "mm", res = 300)
# ggplot()+
#   theme_light()+
#   geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
#   geom_point(data=byhaul %>% filter(Year %in% c(2002:2022)),
#              (aes(ShootLong,ShootLat, colour=scientificname, shape=scientificname,size=sqrt_fishkm2)), alpha=0.5) + #scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
#   geom_point(data=stations %>% filter(Year %in% c(2002:2022)), aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
#   facet_wrap(~Year,dir='h', nrow = 5, ncol = 5)+
#   labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "CPUE by haul", sep=" "), fill="CPUE (sqrt n/km^2)") +
#   coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
#   theme(legend.title = element_text(face = "italic", size = 8))+ theme(legend.position="right", axis.title = element_text(size = 8))
# dev.off()  

#### PLOT only the last ten years  CPUE by haul, remember to change years###
# Scyliorhinus
jpeg(file=paste("Scyliorhinus canicula_by_year.jpeg",sep=""),width = 250, height = 250, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") + #plot the land
  geom_point(data=stations %>% filter(Year %in% c(2014:2023)), aes(ShootLong,ShootLat), shape='.', size=0.05) +   #plot the fished stations
  geom_point(data=byhaul %>% filter(Year %in% c(2014:2023), scientificname=="Scyliorhinus canicula"),
             (aes(ShootLong,ShootLat,size=sqrt_fishkm2)), color="red",alpha=0.5) +  #plot catches by haul
  facet_wrap(~Year,dir='h', nrow = 3, ncol = 4)+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = "Scyliorhinus canicula CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18))+ theme(legend.position=c(0.75,0.1),legend.direction = "horizontal", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(colour = guide_legend(override.aes = list(size = 8)))
dev.off()  

# Mustelus
jpeg(file=paste("Mustelus_map_by_year.jpeg",sep=""),width = 250, height = 250, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") + #plot the land
  geom_point(data=stations %>% filter(Year %in% c(2014:2023)), aes(ShootLong,ShootLat), shape='.', size=0.05) +   #plot the fished stations
  geom_point(data=byhaul %>% filter(Year %in% c(2014:2023), scientificname=="Mustelus"),
             (aes(ShootLong,ShootLat,size=sqrt_fishkm2)), color="red",alpha=0.5) +  #plot catches by haul
  #scale_shape_manual(values=c(15,8,17,16,18,10,7)) +
  facet_wrap(~Year,dir='h', nrow = 3, ncol = 4)+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = "Mustelus spp. CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18))+ theme(legend.position=c(0.75,0.2),legend.direction = "horizontal", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(colour = guide_legend(override.aes = list(size = 10)))
dev.off()  

# Other sharks
jpeg(file=paste("Other_sharks_by_year.jpeg",sep=""),width = 250, height = 300, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") + #plot the land
  geom_point(data=stations %>% filter(Year %in% c(2014:2023)), aes(ShootLong,ShootLat), shape='.', size=0.05) +   #plot the fished stations
  geom_point(data=byhaul %>% filter(Year %in% c(2014:2023), scientificname %in% c("Galeorhinus galeus","Scyliorhinus stellaris", "Squalus acanthias")),(aes(ShootLong,ShootLat,color=scientificname, size=sqrt_fishkm2)), alpha=0.5) +  #plot catches by haul
  facet_wrap(~Year,dir='h', nrow = 3, ncol = 4)+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = "Other sharks CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18))+ theme(legend.position=c(0.8,0.115),legend.direction = "vertical", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(colour = guide_legend(override.aes = list(size = 8)))
dev.off()  	

#### Rays ### 
## note to change years to years of interest
jpeg(file=paste("rajas_map_by_year.jpeg",sep=""),width = 350, height = 350, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaul %>% filter(Year %in% c(2014:2023),scientificname %in% rays),
             (aes(ShootLong, ShootLat, colour=scientificname, shape=scientificname,size=sqrt_fishkm2)), alpha=0.5) + #plot catches by haul
  geom_point(data=stations %>% filter(Year %in% c(2014:2023)), aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  facet_wrap(~Year,dir='h', nrow = 3, ncol = 4)+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = "Rajas CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18))+ 
  theme(legend.position=c(0.75,0.2),legend.direction = "horizontal", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(shape = guide_legend(nrow=4, override.aes = list(size = 8)))+ scale_shape_manual(values=c(15,8,17,16,18,10,7))
dev.off()  

###################
### Map Tot Area ##
###################
# Calculate numbers per station
SpecNotot <- datspec %>% 
  group_by( Survey, HaulNo, ShootLat, ShootLong, SubFactor, sweptarea2, scientificname,TotalNo) %>% 
  #filter(HLNoAtLngt>0 & SubFactor>0) %>% 
  dplyr::summarize(No = sum(HLNoAtLngt)) %>% 
  dplyr::mutate(TotalNo1 = ifelse(TotalNo>0, TotalNo,  #if TotalNo is filled in than take this value
                                  ifelse(No>0 & SubFactor>0, No*SubFactor, NA))) # when TotalNo not filled in, calculate TotalNo based on HLNoAtLngt and SubFactor

# Calculate CPUE (numbers/km2) by haul
byhaultot <- SpecNotot %>% 
  mutate(fishkm2 = TotalNo1/sweptarea2) %>% 
  mutate(sqrt_fishkm2 = sqrt(fishkm2))

#### Rays, PLOT not required for SHARKS in the report #### 
jpeg(file=paste("raja_map_TOT.jpeg",sep=""),width = 300, height = 400, units = "mm", res = 300)
  ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaultot %>% filter(scientificname %in% rays),
             (aes(ShootLong,ShootLat, colour=scientificname,shape=scientificname, size=sqrt_fishkm2)), alpha=0.5) + scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
  geom_point(data=stations , aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = "Rajas CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18)) + theme(legend.position="bottom", axis.title = element_text(size = 18))+
  theme(legend.text=element_text(face="italic",size=16))+ guides(shape = guide_legend(nrow=4, override.aes = list(size = 8)))
dev.off()

##### Sharks, PLOT not required for RAYS in the report #####



jpeg(file=paste("sharks_map_TOT_by_species.jpeg",sep=""),width = 300, height = 300, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaultot %>% filter(scientificname %in% sharks),
             (aes(ShootLong,ShootLat, colour=scientificname,shape=scientificname, size=sqrt_fishkm2)), alpha=0.5) + scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
  geom_point(data=stations , aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  facet_wrap(~scientificname) +
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title ="Sharks CPUE by haul", fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  theme(text=element_text(size=22))+
  theme(legend.title = element_text(face = "bold", size = 18))+ theme(legend.position=c(0.88,0.2), axis.title = element_text(size = 18))+ 
  theme(legend.text=element_text(face = "italic",size=16))+ guides(shape = guide_legend (override.aes = list(size = 8)))
dev.off()
