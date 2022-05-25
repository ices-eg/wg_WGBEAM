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

# load libraries
library(icesDatras)
library(tidyverse)
library(dplyr)
library(tidyr)

##########################################
##########################################
## Get data from DATRAS
##########################################
##########################################
## Get station (HH) data from Datras 
getSurveyList() # gives overview of all survey acronyms

tmpDYFS<-getDATRAS(record = "HH",
               survey = "DYFS",   #only 1 survey at a time
               years= 2000:2021,
               quarters = 1:4) 
# look what's there
head(tmpDYFS)
table(tmpDYFS$Country, tmpDYFS$GearEx);table(tmpDYFS$GearEx, tmpDYFS$Year)
table(tmpDYFS$Country, tmpDYFS$Year)

tmpBTS<-getDATRAS(record = "HH",
                   survey = "BTS",   #only 1 survey at a time
                   years= 2000:2021,
                   quarters = 1:4) 
# look what's there
head(tmpBTS)
table(tmpBTS$Country, tmpBTS$GearEx);table(tmpBTS$GearEx, tmpBTS$Year)
table(tmpBTS$Country, tmpBTS$Year)

tmpSNS<-getDATRAS(record = "HH",
                  survey = "SNS",   #only 1 survey at a time
                  years= 2000:2021,
                  quarters = 1:4) 
# look what's there
head(tmpSNS)
table(tmpSNS$Country, tmpSNS$GearEx);table(tmpSNS$GearEx, tmpSNS$Year)
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
tmpfDYFS <-getDATRAS(record = "HL",
                survey = "DYFS",   #only 1 survey at a time
                years= 2000:2021,
                quarters = 1:4)  
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfDYFS)
table(tmpfDYFS$Country, tmpfDYFS$Year)

## BTS
tmpfBTS <-getDATRAS(record = "HL",
                     survey = "BTS",   #only 1 survey at a time
                     years= 2000:2021,
                     quarters = 1:4) # time to download 10:42 to 11:47
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfBTS)
table(tmpfBTS$Country, tmpfBTS$Year)

## SNS
tmpfSNS <-getDATRAS(record = "HL",
                    survey = "SNS",   #only 1 survey at a time
                    years= 2000:2021,
                    quarters = 1:4) 
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfSNS)
table(tmpfSNS$Country, tmpfSNS$Year)

# select useful colums DYFS
tmpf1<-tmpfDYFS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
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
sp <-worms::wormsbyid(unique(dat$Valid_Aphia))
## Tidy up the species list
sp1<-sp %>% select(Valid_Aphia=AphiaID,scientificname)

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
table(dat1$HaulVal, dat1$Survey);table(dat1$HaulVal, dat1$Year)
dat2 <- dat1  %>% filter(HaulVal == "V") 
table(dat2$HaulVal, dat2$Year)
table(dat2$HaulVal, dat2$Survey)
## Get width of gear from gear codes
# look which gearcodes are in the data
table(dat2$Gear)

# crate new column beamwidth 
# Beam width is the number stated in Gear, so we extract that number into a new variable #Beam_width

dat2$beamwidth <- substr(dat2$Gear, start = 3, stop = 3)


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
setwd("C:/Users/f.masnadi/Desktop/BIOLOGIA della PESCA/CNR/WGBEAM/wg_WGBEAM/elasmobranchs")

# Option 1: based on beam width and distance
# check if there are missing records for Distance
load("dat2000_II.rda")
test <- dat %>%  filter(is.na(Distance))
table(test$Country, test$Year)

#When distance is NA, then calculate it as 1853*HaulDur/60
dat <- transform(dat, DeriveDistance = ifelse(!is.na(Distance), Distance, (1853*HaulDur)/60)) 

#Calculate swept area as Distance* beamwidth
dat <- dat %>% mutate(SweptArea_m2 = as.numeric(DeriveDistance) * as.numeric(beamwidth))

#To kilometers 
dat <- dat %>% mutate(SweptArea_km2 = SweptArea_m2/1000000)


# When there is no distance recorded:
# Option 2: calculate swept area based on beam width, ground speed and haulduration
#table(dat$GroundSpeed) 
#dat$GroundSpeed[dat$GroundSpeed == -9]<- 3.5 ## Set speed to average 3.5 knots when not recorded
# create column swept area
#dat<-dat %>% 
#  mutate(sweptarea2 = ((beamwidth/1000)*(1.852*GroundSpeed*HaulDur/60))) ## Calculate swept area in km2


# make a list of the fished stations in the survey
stations_fra <- dat %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, SweptArea_km2) %>% 
  distinct()
##########################################
##########################################
## Select species ##
##########################################
##########################################
ispec <- "Raja familiy"

datspec<-dat %>% 
  filter(Valid_Aphia %in% c(105883,105891,367297,105885,105887,105865,105876))

# calculate number of speciments that were measured
number_measured <- datspec %>% 
  select(Country, Ship, Survey, Gear, Year, HaulNo, ShootLat, ShootLong, scientificname,NoMeas) %>%
  distinct() 

number_measured <- number_measured %>% 
  group_by(Country, Ship, Survey, Gear,Year,scientificname) %>% dplyr::summarize(NumMeas = sum(NoMeas))

# put = instead of NA
number_measured[is.na(number_measured)] <- 0


# Calculate numbers per station
SpecNo <- datspec %>% 
  group_by(Country, Ship, Survey, Gear, Year, HaulNo, ShootLat, ShootLong, SubFactor, SweptArea_km2, scientificname,TotalNo) %>% 
  #filter(HLNoAtLngt>0 & SubFactor>0) %>% 
  dplyr::summarize(No = sum(HLNoAtLngt)) %>% 
  dplyr::mutate(TotalNo1 = ifelse(TotalNo>0, TotalNo,  #if TotalNo is filled in than take this value
                           ifelse(No>0 & SubFactor>0, No*SubFactor, NA))) # when TotalNo not filled in, calculate TotalNo based on HLNoAtLngt and SubFactor

jpeg(file=paste("raja_totN_NEW.jpeg",sep=""),width = 200, height = 200, units = "mm", res = 400)
ggplot(SpecNo)+
  geom_bar(aes(x=Year, y=TotalNo1, fill=Country),stat = "identity")+
  facet_wrap(~scientificname, scale = "free")
dev.off()


##########################################
##########################################
## CPUE
##########################################
##########################################
# Calculate CPUE (numbers/km2) by haul
byhaul <- SpecNo %>% 
  mutate(fishkm2 = TotalNo1/SweptArea_km2) %>% 
  mutate(sqrt_fishkm2 = sqrt(fishkm2))

### Plot catches
## Map by Haul ##

#first, make map of the north sea #
library(mapdata)
m <-map_data("worldHires") 

## set map boundaries
xmin <-  min(stations$ShootLong) # -11
xmax <-  max(stations$ShootLong)   #10
ymin <- 48.5    # using min(stations$ShootLat) there is a wrong Lat data in 2012
ymax <- max(stations$ShootLat) #61

###################
### Map by Year ##
###################
## note to change years to years of interest
jpeg(file=paste("raja_map_by_year.jpeg",sep=""),width = 400, height = 320, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaul %>% filter(Year %in% c(2002:2021)),
             (aes(ShootLong,ShootLat, colour=scientificname, shape=scientificname,size=sqrt_fishkm2)), alpha=0.5) + scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
  geom_point(data=stations %>% filter(Year %in% c(2002:2021)), aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  facet_wrap(~Year,dir='h', nrow = 5, ncol = 5)+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "CPUE by haul", sep=" "), fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(legend.text =  element_text( size = 13), legend.title = element_text(face = "italic", size = 12))+ theme(legend.position="right", axis.title = element_text(size = 13))
dev.off()  

###################
### Map Tot Area ##
###################
# Calculate numbers per station
SpecNotot <- datspec %>% 
  group_by( Survey, HaulNo, ShootLat, ShootLong, SubFactor, SweptArea_km2, scientificname,TotalNo) %>% 
  #filter(HLNoAtLngt>0 & SubFactor>0) %>% 
  dplyr::summarize(No = sum(HLNoAtLngt)) %>% 
  dplyr::mutate(TotalNo1 = ifelse(TotalNo>0, TotalNo,  #if TotalNo is filled in than take this value
                                  ifelse(No>0 & SubFactor>0, No*SubFactor, NA))) # when TotalNo not filled in, calculate TotalNo based on HLNoAtLngt and SubFactor

# Calculate CPUE (numbers/km2) by haul
byhaultot <- SpecNotot %>% 
  mutate(fishkm2 = TotalNo1/SweptArea_km2) %>% 
  mutate(sqrt_fishkm2 = sqrt(fishkm2))

jpeg(file=paste("raja_map_TOT.jpeg",sep=""),width = 300, height = 240, units = "mm", res = 300)
  ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaultot ,
             (aes(ShootLong,ShootLat, colour=scientificname,shape=scientificname, size=sqrt_fishkm2)), alpha=0.5) + scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
  geom_point(data=stations , aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "CPUE by haul", sep=" "), fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(legend.title = element_text(face = "italic", size = 8)) + theme(legend.position="right", axis.title = element_text(size = 8)) 
dev.off()


###################
### Map Tot by species ##
###################
jpeg(file=paste("raja_map_by_species.jpeg",sep=""),width = 410, height = 300, units = "mm", res = 300)
ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaultot ,
             (aes(ShootLong,ShootLat, colour=scientificname,shape=scientificname, size=sqrt_fishkm2)), alpha=0.5) + scale_shape_manual(values=c(15,8,17,16,18,10,7)) +  #plot catches by haul
  geom_point(data=stations , aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  facet_wrap(~scientificname,dir='h', nrow = 3, ncol = 3) +
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "CPUE by haul", sep=" "), fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(legend.text =  element_text( size = 12),legend.title = element_text(face = "italic", size = 8)) + theme(legend.position="right", axis.title = element_text(size = 12)) 
dev.off()
