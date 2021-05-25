##################################################################################
# 
# WGBEAM Tor a, b (1): Evaluate combined offshore and inshore data by region, 
# and cross-regionally where relevant, for species used in fish stock assessment 
# incl. elasmobranchs and brown shrimp
#
# 
# Created by Loes, Gudjon, Giuseppe & Vaishav
# 25/03/2020
#
# Updated on 26/03/2021 by Loes
#################################################################################
# clear environment
rm(list=ls()) 

# load libraries
library(icesDatras)
library(tidyverse)
library(dplyr)
library(tidyr)

isurvey <-"DYFS"
##########################################
##########################################
## Get data from DATRAS
##########################################
##########################################
## Get station (HH) data from Datras 
getSurveyList() # gives overview of all survey acronyms

tmp<-getDATRAS(record = "HH",
               survey = "DYFS",   #only 1 survey at a time
               years= 2004:2020,
               quarters = 1:4)

# look what's there
head(tmp)
table(tmp$Year)
table(tmp$Quarter)
table(tmp$Country)
table(tmp$Country, tmp$Year)

# select useful colums
tmp1<-tmp %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                     Stratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                     Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                     SurTemp, BotTemp, SurSal, BotSal)

# look for NAs
for (Var in names(tmp1)) {
  missing <- sum(is.na(tmp1[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

## Get length (HL) data from Datras
tmpf<-getDATRAS(record = "HL",
                survey = "DYFS",   #only 1 survey at a time
                years= 2004:2020,
                quarters = 1:4)

# look what's there
head(tmpf)
table(tmpf$Year)
table(tmpf$Quarter)
table(tmpf$Country)
table(tmpf$Country, tmpf$Year)

# select useful colums
tmpf1<-tmpf %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                       SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                       SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, Valid_Aphia)

# look for NAs
for (Var in names(tmpf1)) {
  missing <- sum(is.na(tmpf1[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

## Get age (CA) data from Datras
tmpa<-getDATRAS(record = "CA",
                survey = "DYFS",   #only 1 survey at a time
                years= 2004:2020,
                quarters = 1:4)

# look whats there
table(tmpa$Country, tmpa$Year)
table(tmpa$SpecCode, tmpa$Country)

# select useful colums
tmpa1 <- tmpa %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                         SpecCode, AreaCode, LngtCode, LngtClass, Sex, Maturity,
                         Age, NoAtALK, IndWgt, Valid_Aphia)

##########################################
##########################################
## Create one dataset
##########################################
##########################################
## Joining HH en HL datasets 
dat <-tmpf1 %>%
  left_join(tmp1) 

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
dat2 <- dat1 %>% 
  filter(HaulVal == "V")

## Get width of gear from gear codes
# look which gearcodes are in the data
table(dat2$Gear)

# crate new column beamwidth 
dat2 <- dat2 %>% 
  mutate(beamwidth = case_when(Gear == "BT8" ~ 8,
                               Gear == "BT7" ~ 7,
                               Gear == "BT6" ~ 6,
                               Gear == "BT4A" ~ 4,
                               Gear == "BT4AI" ~ 4,
                               Gear == "BT4S" ~ 4,
                               Gear == "BT3" ~ 3,
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
#save(dat, file = "dat.rda")
load("dat.rda")

##########################################
##########################################
## Calculate swept area
##########################################
##########################################
# Option 1: based on beam width and distance
# check if there are missing records for Distance
test <- dat %>%  filter(is.na(Distance))
table(test$Country, test$Year)

# look at the relationship HaulDur vs. Distance
ggplot(dat)+
  geom_point(aes(x=HaulDur, y=Distance, color=Country))

# save plot
# setwd(outputpath)
# ggsave(filename = paste(isurvey, "hauldur_distance.png", sep="_"), plot = last_plot(), width=10,height=8, dpi=600)

# based on the plot above chose if you want to remove the outliers
dat <- dat %>% 
  filter(HaulDur < 40) %>% 
  filter(Distance < 4000)

# create column swept area
dat<-dat %>% 
  mutate(sweptarea1 = beamwidth*Distance/1000000) ## Calculate per swept area in km2

# When there is no distance recorded:
# Option 2: calculate swept area based on beam width, ground speed and haulduration
table(dat$GroundSpeed) 
dat$GroundSpeed[dat$GroundSpeed == -9]<- 3.5 ## Set speed to average 3.5 knots when not recorded
# create column swept area
dat<-dat %>% 
  mutate(sweptarea2 = ((beamwidth/1000)*(1.852*GroundSpeed*HaulDur/60))) ## Calculate swept area in km2

# look for NAs
for (Var in names(dat)) {
  missing <- sum(is.na(dat[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

look at difference between methodologies for swept area calculation??
ggplot(dat)+
  geom_point(aes(x=sweptarea1, y=sweptarea2, color=Country))

ggplot(dat)+
  geom_point(aes(x=sweptarea2, y=sweptarea1, color=Country))

# save plot
# setwd(outputpath)
# ggsave(filename = paste(isurvey, "swept_area_comparison.png", sep="_"), plot = last_plot(), width=10,height=8, dpi=600)

# make a list of the fished stations in the survey
stations <- dat %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, sweptarea2) %>% 
  distinct()

##########################################
##########################################
## Select species ##
##########################################
##########################################
ispec <- "Crangon crangon"

datspec<-dat %>% 
  filter(scientificname == ispec) 

# look for NAs
for (Var in names(datspec)) {
  missing <- sum(is.na(datspec[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

str(dat)

# calculate number of shrimp that were measured
number_measured <- datspec %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, NoMeas) %>%
  distinct() 

number_measured <- number_measured %>% 
  group_by(Country, Ship, Survey, Gear, Year) %>% 
  summarize(NumMeas = sum(NoMeas))

ggplot(number_measured %>% filter(Year %in% c(2017:2020)))+
  geom_bar(aes(x=Year, y=NumMeas, fill=Country), stat = "identity", position="dodge") +
  facet_wrap(Country~Ship)

number_measured_last3years <- number_measured %>% 
  filter(Year %in% c(2017:2020))

number_measured_last3years[is.na(number_measured_last3years)] <- 0

# write table
# setwd(outputpath)
# write.csv(number_measured_last3years, file=paste(ispec, "number_measured.csv"))

# save plot
# setwd(outputpath)
# ggsave(filename = paste(ispec, "number_measured.png", sep="_"), plot = last_plot(), width=10,height=8, dpi=600)


# Calculate numbers per station
SpecNo <- datspec %>% 
  group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, SubFactor, sweptarea2, TotalNo) %>% 
  #filter(HLNoAtLngt>0 & SubFactor>0) %>% 
  summarize(No = sum(HLNoAtLngt)) %>% 
  mutate(TotalNo1 = ifelse(TotalNo>0, TotalNo,  #if TotalNo is filled in than take this value
                          ifelse(No>0 & SubFactor>0, No*SubFactor, NA))) # when TotalNo not filled in, calculate TotalNo based on HLNoAtLngt and SubFactor

# look for NAs
for (Var in names(SpecNo)) {
  missing <- sum(is.na(SpecNo[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

ggplot(SpecNo)+
  geom_bar(aes(x=Year, y=TotalNo1, fill=Country),stat = "identity")+
  facet_wrap(~Ship)

# setwd(outputpath)
# ggsave(filename = paste(ispec, "total_numbers.png"), plot = last_plot(), width=10,height=8, dpi=600)

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
ymin <- min(stations$ShootLat)
ymax <- max(stations$ShootLat)

### plot the map with the data ##
## note to change years to years of interest
mapbyhaul <- 
  ggplot()+
  theme_light()+
  geom_polygon(data=m,aes(long,lat,group=group),fill=NA,color="grey") +  #plot the land
  geom_point(data=byhaul %>% filter(Year %in% c(2015:2020)),
             (aes(ShootLong,ShootLat, colour=Country, size=sqrt_fishkm2)), alpha=0.4) +  #plot catches by haul
  geom_point(data=stations %>% filter(Year %in% c(2015:2020)), aes(ShootLong,ShootLat), shape='.', size=0.05) +  #plot the fished stations
  facet_wrap(~Year,dir='v')+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "CPUE by haul", sep=" "), fill="CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(legend.title = element_text(face = "italic", size = 8))

# mapbyhaul
# setwd(outputpath)
# ggsave(filename = paste(ispec, "CPUE_by_haul.png"), plot = last_plot(), width=8,height=10, dpi=600)


## Map by stat rectangle ##

# install geo package
# devtools::install_github("hafro/geo")

## ADD STATIONS WITH ZERO OBSERVATIONS!
byhaul_stat <- stations %>% 
  left_join(byhaul) 

# look for NAs
for (Var in names(byhaul_stat)) {
  missing <- sum(is.na(byhaul_stat[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# Replace NAs with zero observations
byhaul_stat$fishkm2[is.na(byhaul_stat$fishkm2)]<- 0
byhaul_stat$sqrt_fishkm2[is.na(byhaul_stat$sqrt_fishkm2)]<- 0

# add StatRec where missing
byhaul_stat <- byhaul_stat %>% 
  mutate(StatRec = ifelse(!is.na(StatRec), 
                          StatRec,geo::d2ir(ShootLat, ShootLong)))

## Calculate mean catch per rectangle
bystatrec<-byhaul_stat %>% 
  group_by(Country,Year,Ship, Survey, StatRec) %>% 
  summarize(mean_fishkm2=mean(fishkm2), 
            mean_sqrt_fishkm2=mean(sqrt_fishkm2)) %>% 
  mutate(lon = geo::ir2d(StatRec)$lon,
         lat = geo::ir2d(StatRec)$lat) 

## Breaks in data for scale in plot
my_breaks<-c(0,100, 200, 300, 400, 500,1000,1500,2000,2500,3000)

#Plot the map with the data ##
mapbystatrec <-
ggplot()+
  theme_light()+
  geom_tile(data=bystatrec %>%  filter(Year %in% c(2016:2019)),aes(lon,lat,fill=mean_sqrt_fishkm2)) + #color statrec tiles based on mean cpue values
  scale_fill_continuous(low="yellow",high="red",trans="log")+
  geom_polygon(data=m,aes(long,lat,group=group), fill="grey") +  #plot the land
  geom_point(data=stations %>% filter(Year %in% c(2016:2019)),aes(ShootLong,ShootLat),shape=".")+ #plot fished stations
  facet_wrap(~Year,dir='v')+
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste(ispec, "mean CPUE by StatRec", sep=" "), fill="Mean CPUE (sqrt n/km^2)") +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(legend.title = element_text(face = "italic", size = 8)) +
  labs()

mapbystatrec
# setwd(outputpath)
# ggsave(filename = paste(ispec, "CPUE_by_statrec.png"), plot = last_plot(), width=8,height=10, dpi=600)

##########################################
##########################################
## Length distributions
##########################################
##########################################
LFD <- datspec %>% 
  filter(HLNoAtLngt >0 ) %>%
  filter(HaulVal=="V") %>%  # take only valid hauls into account
  group_by(Year, Survey, Country, Length) %>% 
  summarize(lenf=sum(HLNoAtLngt), 
            raised_lenf=sum(HLNoAtLngt*SubFactor))

#look for outliers
ggplot(LFD)+
  geom_point(aes(x=Length, y=lenf, color=Country))

# setwd(outputpath)
# ggsave(filename = paste(ispec, "lengths.png"), plot = last_plot(), width=10,height=8, dpi=600)

# delete outliers
LFD <- LFD %>% 
  filter(Length<= 150 & Length >=10) # chose where to cut off lengts

# plot LFD
plotLFD <- ggplot(data=LFD, aes(x=Length, y=lenf)) + geom_line(size=1)
plotLFD <- plotLFD +   facet_grid(Year~Country) + labs(title = "Raised number shrimp DYFS") 
plotLFD

# setwd(outputpath)
# ggsave(filename = paste(ispec, "LFD_by_year.png"), plot = last_plot(), width=10,height=8, dpi=600)

#plot raised LFD
plotLFD <- ggplot(data=LFD, aes(x=Length, y=raised_lenf)) + geom_line(size=1)
plotLFD <- plotLFD +   facet_grid(Year~Country) + labs(title = "Raised number shrimp DYFS") 
plotLFD

# setwd(outputpath)
# ggsave(filename = paste(ispec, "raised_LFD_by_year.png"), plot = last_plot(), width=10,height=8, dpi=600)

# standardise to swept area
sweptarea <- datspec %>% 
  filter(HLNoAtLngt >0 ) %>%
  filter(HaulVal=="V") %>%  # take only valid hauls into account
  select(Year, Survey, Country, StNo, HaulNo, sweptarea2) %>% 
  distinct() %>% 
  group_by(Year, Survey, Country) %>% 
  summarize(SA=sum(sweptarea2))

LFD.std <- LFD %>% 
  left_join(sweptarea) %>% 
  mutate(lenf.std=raised_lenf/SA)

plotLFD2 <- ggplot(data=LFD.std, aes(x=Length, y=lenf.std)) + geom_line(size=1)
plotLFD2 <- plotLFD2 +   facet_grid(Year~Country) 
plotLFD2

plotLFD3 <- ggplot(data=LFD.std %>%  filter(Year %in% c(2012:2020)), aes(x=Length, y=lenf.std)) + geom_line(size=1, aes(color=Country))
plotLFD3 <- plotLFD3 +   facet_grid(Survey~Year) 
plotLFD3

# setwd(outputpath)
# ggsave(filename = paste(ispec, "std_raised_LFD_by_year.png"), plot = last_plot(), width=10,height=8, dpi=600)

##########################################
##########################################
## LWK & ALK
##########################################
##########################################
# to be continued

