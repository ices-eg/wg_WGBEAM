##################################################################################
# 
# WGBEAM: Evaluate spatial-temporal distribution from combined offshore and inshore data for cephalopods 
# 
# Original script based on elasmobranch script 
# Adapted by Lies Vansteenbrugge (24/03/2026)
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
library(worrms)
library(mapdata)

# load from line 333
##########################################
##########################################
## Get data from DATRAS
##########################################
##########################################
## Get station (HH) data from Datras 
getSurveyList() # gives overview of all survey acronyms

tmpDYFS<-getDATRAS(record = "HH",
                   survey = "DYFS",   #only 1 survey at a time
                   years= 2000:2025,
                   quarters = 1:4) 
# look what's there
head(tmpDYFS)
table(tmpDYFS$Country, tmpDYFS$Year)


tmpBTS<-getDATRAS(record = "HH",
                  survey = "BTS",   #only 1 survey at a time
                  years= 2000:2025,
                  quarters = 1:4) 
# look what's there
head(tmpBTS)
table(tmpBTS$Country, tmpBTS$Year)


tmpSNS<-getDATRAS(record = "HH",
                  survey = "SNS",   #only 1 survey at a time
                  years= 2000:2025, #2003 not available
                  quarters = 1:4) 

head(tmpSNS)
table(tmpSNS$Country, tmpSNS$Year)


tmp8<-getDATRAS(record = "HH",
                survey = "BTS-VIII",   #only 1 survey at a time
                years= 2000:2025,
                quarters = 1:4) 
# look what's there
head(tmp8)
table(tmp8$Country, tmp8$Year) # data from 2007


# select useful columns DYFS
tmp1<-tmpDYFS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                         DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                         Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                         SurTemp, BotTemp, SurSal, BotSal)
# select useful columns BTS
tmp2<-tmpBTS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                        DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                        Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                        SurTemp, BotTemp, SurSal, BotSal)

# select useful columns SNS
tmp3<-tmpSNS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                        DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                        Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                        SurTemp, BotTemp, SurSal, BotSal)

# select useful columns BTS-VIII
tmp4<-tmp8 %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, Month, Day, 
                      DepthStratum, HaulDur,DayNight,ShootLat, ShootLong, HaulLat, HaulLong, StatRec, 
                      Depth, HaulVal, DataType, Rigging, Tickler, Distance, GroundSpeed, 
                      SurTemp, BotTemp, SurSal, BotSal)




###########
## Joining DYFS and BTS and SNS and BTS-VIII
###########
HHFIN <- rbind(tmp1,tmp2,tmp3,tmp4)

head(HHFIN)
table(HHFIN$Quarter, useNA = "always")

# look for NAs
for (Var in names(HHFIN)) {
  missing <- sum(is.na(HHFIN[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
} #NA's in StNo; DepthStratum; HaulDur; StatRec; Depth; Rigging; Tickler; Distance


######
## Get length (HL) data from Datras
# DYFS
Sys.time() 
tmpfDYFS1 <-getDATRAS(record = "HL",
                      survey = "DYFS",   #only 1 survey at a time
                      years= 2000:2025,
                      quarters = 1:4)  
Sys.time() # takes 5 minutes
# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there
head(tmpfDYFS1)
table(tmpfDYFS1$Country, tmpfDYFS1$Year)


## BTS
Sys.time() 
tmpfBTS <-getDATRAS(record = "HL",
                    survey = "BTS",   #only 1 survey at a time
                    years= 2000:2025,
                    quarters = 1:4) # time to download 7 min 
Sys.time() 
# look what's there
head(tmpfBTS)
table(tmpfBTS$Country, tmpfBTS$Year)


## SNS
Sys.time() 
tmpfSNS <-getDATRAS(record = "HL",
                    survey = "SNS",   #only 1 survey at a time
                    years= 2000:2025,
                    quarters = 1:4) # time to download 3 minutes
Sys.time() 
head(tmpfSNS)
table(tmpfSNS$Country, tmpfSNS$Year)


# 8ab survey
Sys.time() 
tmpf8ab <-getDATRAS(record = "HL",
                    survey = "BTS-VIII",   #only 1 survey at a time
                    years= 2000:2025,
                    quarters = 1:4)  
Sys.time() 
head(tmpf8ab)
table(tmpf8ab$Country, tmpf8ab$Year) # data from 2007


# if the getDATRAS is too slow download the data directly from https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
# look what's there


# select useful colums DYFS
tmpf1<-tmpfDYFS1 %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                            SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                            SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, DevStage, Valid_Aphia)
# select useful colums BTS
tmpf2<-tmpfBTS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                          SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                          SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, DevStage, Valid_Aphia)
# select useful colums SNS
tmpf3<-tmpfSNS %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                          SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                          SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, DevStage, Valid_Aphia)

tmpf4<-tmpf8ab %>% select(Year, Quarter, Country, Ship, Survey, Gear, StNo, HaulNo, 
                          SpecCode, SpecVal, Sex, TotalNo, CatIdentifier, NoMeas, 
                          SubFactor, SubWgt, CatCatchWgt, LngtCode, LngtClass, HLNoAtLngt, DevStage, Valid_Aphia)

###########
## Joining DYFS and BTS and SNS anmd BTS-VIII
###########
HLFIN <- rbind(tmpf1,tmpf2,tmpf3,tmpf4)
# look for NAs
for (Var in names(HLFIN)) {
  missing <- sum(is.na(HLFIN[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
} #NA's in StNo; Sex; CatIdentifier; NoMeas; SubWgt; CatCatchWgt; LngtCode; LngtClass; DevStage


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

save(dat, file = "./ADHOC/INPUT/WGBEAM2026/ceph_dat2000_25_1.rda")

## Create a species list from the aphia codes
sp <-dat %>%
  distinct(Valid_Aphia)
sp$scientificname <- "NA"

for(i in 1:length(sp$Valid_Aphia)){
  sp$scientificname[i] <- worrms::wm_id2name(sp$Valid_Aphia[i])
}

head(sp)

library(readxl)
cephalopods_aphia <- read_excel("./ADHOC/INPUT/WGBEAM2026/cephalopods_aphia.xlsx")
head(cephalopods_aphia)
cephalopods_aphia$Valid_Aphia <- cephalopods_aphia$APHIA_ID_VALID
# test <- cephalopods_aphia %>% 
#   filter(APHIA_ID_VALID == 11760)


# couple the families to the species so you can filter later
sp2 <- sp %>% 
  left_join(cephalopods_aphia %>% select(Valid_Aphia, APHIA_FAMILY), by = "Valid_Aphia")

head(sp2)



# ## Tidy up the species list
# sp1<-sp %>% select(Valid_Aphia,scientificname)
#
## Get species names from worms
head(dat)
nrow(dat) #2518606
nrow(sp2) #1088
length(unique(sp2$Valid_Aphia)) #1088

# dupes <- sp2 %>%
#   count(Valid_Aphia) %>%
#   filter(n > 1)

dat1 <- dat %>% 
  left_join(sp2, by = "Valid_Aphia") %>% 
  filter(!is.na(APHIA_FAMILY)) #39481 records

head(dat1)
nrow(dat1)

# table(dat1$Year, is.na(dat1$Valid_Aphia), useNA = "always")

# look for NAs
for (Var in names(dat1)) {
  missing <- sum(is.na(dat1[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# only take data from valid hauls
table(dat1$HaulVal, useNA = "always")

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

# set all length measurements in mm
table(dat2$LngtCode)
dat2 <- dat2 %>% 
  mutate(Length = ifelse (LngtCode %in% c("1", "5"), LngtClass*10, LngtClass))

# rename dataset to dat
dat <- dat2  

#save dat 
save(dat, file = "./ADHOC/INPUT/WGBEAM2026/ceph_dat2000_25.rda")



##########################################
##########################################
## Calculate swept area
##########################################
##########################################

# Option 1: based on beam width and distance
# check if there are missing records for Distance
load("./ADHOC/INPUT/WGBEAM2026/ceph_dat2000_25.rda") #use to upload save file to avoid reload data in case you close the session
test <- dat %>% filter(is.na(Distance))
table(dat$Distance, useNA = "always") 
table(test$Country, test$Year, useNA = "always")

# When there is no distance recorded:
# Option 2: calculate swept area based on beam width, ground speed and haul duration
# Option 3: use earth curvation calculation method
# Option 4: use default groundspeed: BTS 4 knots; DYFS 2.5 knots ; SNS 3.5 knots

table(dat$GroundSpeed, useNA = "always") 

head(dat)

radians <- function(x) x * pi / 180

dat2 <- dat %>% 
  mutate(Distance_3 = 1.852* 360 * (60/(2*pi))*
         acos(
           cos(radians(ShootLat))*cos(radians(HaulLat))*
             cos(radians(HaulLong)-radians(ShootLong))+
             sin(radians(ShootLat))*sin(radians(HaulLat)))) %>% 
  mutate(Distance_2 = (HaulDur/60)*1.852*GroundSpeed) %>% 
  mutate(Distance = Distance/1000) %>% 
  mutate(Distance_final = case_when(!is.na(Distance) ~ Distance,
    is.na(Distance) & GroundSpeed != -9 ~ Distance_2,
    is.na(Distance) & GroundSpeed == -9 ~ Distance_3)) %>% 
  mutate(GroundSpeed = case_when(
    Distance_final > 1000 & Survey == "BTS" ~ 4,
    Distance_final > 1000 & Survey == "DYFS" ~ 2.5,
    Distance_final > 1000 & Survey == "SNS" ~ 3.5,
    TRUE ~ GroundSpeed
  )) %>% 
  mutate(Distance_2 = (HaulDur/60) * 1.852 * GroundSpeed) %>% 
  mutate(Distance_final = case_when(
    !is.na(Distance) ~ Distance,
    is.na(Distance) & GroundSpeed != -9 ~ Distance_2,
    is.na(Distance) & GroundSpeed == -9 ~ Distance_3
  )) %>% 
  mutate(sweptarea2 = (beamwidth / 1000) * Distance_final)

# table(dat2$sweptarea2, useNA = "always")
# range(dat2$Distance_3)
# range(dat2$sweptarea2)
# ggplot(dat2, aes(as.factor(Year), sweptarea2))+geom_boxplot()+facet_wrap(~Country)
# ggplot(dat2, aes(as.factor(Year), Distance_final))+geom_boxplot()+facet_wrap(~Country)
# 
# test <- dat2 %>% 
#   filter(sweptarea2 > 0.1)
# write.csv(test, file = "./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/issue_GB_coo.csv")
# 
# ggplot(dat2 %>% filter(Year>2003), aes(as.factor(Year), sweptarea2))+geom_boxplot()+facet_wrap(~Country)



# make a list of the fished stations in the survey
stations <- dat2 %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, sweptarea2) %>%
  distinct()

stationsQ1 <- dat2 %>% 
  filter(Quarter == 1) %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, sweptarea2) %>%
  distinct()

stationsQ34 <- dat2 %>% 
  filter(Quarter %in% c(3,4)) %>% 
  select(Country, Ship, Survey, Gear, Year, StNo, HaulNo, ShootLat, ShootLong, StatRec, sweptarea2) %>%
  distinct()


##########################################
##########################################
## Remove development stage
##########################################
##########################################
table(dat2$DevStage, useNA = "always")

dat2adults <- dat2 %>% 
  filter(is.na(DevStage)) #38418 records


##########################################
##########################################
## Remove night hauls
##########################################
##########################################
table(dat2adults$DayNight, useNA = "always")

dat2adultsday <- dat2adults %>% 
  filter(DayNight == "D") #34809 records



##########################################
##########################################
## Split data by quarter
##########################################
##########################################
datQ1 <- dat2adultsday %>% 
  filter(Quarter == 1)

datQ34 <- dat2adultsday %>% 
  filter(Quarter %in% c(3,4))

head(datQ34) #27899


##################################################### QUARTER 3, 4 ###############################################

##########################################
##########################################
## check if numbers were registered
##########################################
##########################################
table(datQ34$Year, is.na(datQ34$NoMeas), useNA = "always") # over whole time series NA's at numbers measured
table(datQ34$Year, is.na(datQ34$TotalNo), useNA = "always") 
range(datQ34$TotalNo)
test <- datQ34 %>% 
  filter(TotalNo == -9)
table(test$Year) # 31 records without TotalNo -> decided to remove

datQ34onlynum <- datQ34 %>% 
  filter(TotalNo !=-9) #27402 -> lost 31 records








######## use for loop to plot all families: 


# get unique families
families <- unique(datQ34onlynum$APHIA_FAMILY)

# world map
m <- map_data("worldHires")

# map boundaries 
xmin <- min(stationsQ34$ShootLong)
xmax <- max(stationsQ34$ShootLong)
ymin <- 48.5
ymax <- max(stationsQ34$ShootLat)

fam <- "Loliginidae"
for (fam in families) {
  
  cat("Processing:", fam, "\n")
  
  ##########################################
  # Filter data
  ##########################################
  dat_fam <- datQ34onlynum %>% 
    filter(APHIA_FAMILY == fam)
  
  ##########################################
  # Summaries
  ##########################################
  dat2 <- dat_fam %>%
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             Valid_Aphia, ShootLat, ShootLong, scientificname,
             APHIA_FAMILY, sweptarea2) %>%
    summarise(n_TotalNo = n_distinct(TotalNo), .groups = "drop")
  
  
  dat3 <- dat2 %>% 
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             ShootLat, ShootLong, APHIA_FAMILY, sweptarea2) %>% 
    summarise(tot = sum(n_TotalNo), .groups = "drop") 
  
  
  ##########################################
  # PLOTS (Totals)
  ##########################################
  p1 <- ggplot(dat3, aes(x = Year, y = tot, fill = Country)) +
    geom_bar(stat = "identity") +
    ylab("Total number") +
    theme_bw()
  
  ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                fam, "_NbyCountryYear.jpg"),
         plot = p1, width = 8, height = 8, units = "in")
  
  p2 <- ggplot(dat3, aes(x = Year, y = tot)) +
    geom_bar(stat = "identity") +
    ylab("Total number") +
    facet_wrap(~Country)
  
  ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                fam, "_NbyCountryYear2.jpg"),
         plot = p2, width = 8, height = 8, units = "in")

  
  
  ##########################################
  # CPUE
  ##########################################
  dat3 <- dat3 %>% 
    mutate(CPUE = tot / sweptarea2,
           sqrtcpue = sqrt(CPUE))
  
  # head(as.data.frame(dat3))
  # range(dat3$sweptarea2)
  
  # find the max year dynamically
  max_year <- max(dat3$Year, na.rm = TRUE)
  
  # filter the last 10 years
  dat3b <- dat3 %>% 
    filter(CPUE < 1000) %>% 
    filter(Year >= (max_year - 9) & Year <= max_year)
  
  # range(dat3b$Year)
  
  ##########################################
  # MAP (by haul)
  ##########################################
  year_map=c((max(dat$Year)-9):max(dat$Year))
  
  jpeg(file = paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                     fam, "_map.jpg"),
       width = 250, height = 250, units = "mm", res = 300)
  
  print(
    ggplot() +
      theme_light() +
      geom_polygon(data = m, aes(long, lat, group = group),
                   fill = NA, color = "grey") +
      geom_point(data = stationsQ34 %>%
                   filter(Year %in% year_map, Survey != "BTS-VIII"),
                 aes(ShootLong, ShootLat),
                 shape = '.', size = 0.05) +
      geom_point(data = dat3b %>%
                   filter(Year %in% year_map, Survey != "BTS-VIII"),
                 aes(ShootLong, ShootLat, size = CPUE),
                 color = "red", alpha = 0.5) +
      facet_wrap(~Year, dir='h') +
      labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste0(fam, " CPUE by haul"), fill="CPUE (n/km^2)") + 
      coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
      theme(text=element_text(size=22), axis.text = element_text(size = 14)) +  
      theme(legend.title = element_text(face = "bold", size = 18)) + 
      theme(legend.position="bottom",legend.direction = "horizontal", axis.title = element_text(size = 18)) + 
      theme(legend.text=element_text(face = "italic",size=16)) + 
      guides(colour = guide_legend(override.aes = list(size = 8)))
  )
  
  dev.off()
  
  ##########################################
  # BY SPECIES
  ##########################################
  dat4 <- dat2 %>% 
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             ShootLat, ShootLong, APHIA_FAMILY,
             scientificname, sweptarea2) %>% 
    summarise(tot = sum(n_TotalNo), .groups = "drop") %>%
    mutate(CPUE = tot / sweptarea2,
           sqrtcpue = sqrt(CPUE))
  
  dat4b <- dat4 %>% 
    filter(CPUE < 1000)
  
  p3 <- ggplot(dat4b, aes(x = Year, y = tot, fill = Country)) +
    geom_bar(stat = "identity") +
    facet_wrap(~scientificname) + ylab("Total number")+
    theme_bw()
  
  ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                fam, "_NbySpeciesCountry.jpg"),
         plot = p3, width = 8, height = 8)
  
  ##########################################
  # MAP BY SPECIES
  ##########################################
  
  jpeg(file = paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                     fam, "_map_species.jpg"),
       width = 350, height = 350, units = "mm", res = 300)
  
  print(
    ggplot() +
      theme_light() +
      geom_polygon(data = m, aes(long, lat, group = group),
                   fill = NA, color = "grey") +
      geom_point(data = dat4b %>%
                   filter(Year %in% year_map, Survey != "BTS-VIII"),
                 aes(ShootLong, ShootLat,
                     shape=scientificname,
                     colour = scientificname,
                     size = CPUE),
                 alpha = 0.5) +
      geom_point(data = stationsQ34 %>%
                   filter(Year %in% year_map, Survey != "BTS-VIII"),
                 aes(ShootLong, ShootLat),
                 shape = '.', size = 0.05) +
      facet_wrap(~Year, dir='h', nrow = 3, ncol = 4) +
      labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste0(fam," CPUE by haul", fill="CPUE (n/km^2)")) +
      coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
        theme(text=element_text(size=22), axis.text = element_text(size = 14))+
        theme(legend.title = element_text(face = "bold", size = 18))+ 
        theme(legend.position="bottom",legend.direction = "horizontal", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(shape = guide_legend(nrow=4, override.aes = list(size = 8)))+ scale_shape_manual(values=c(15,8,17,16,18,10,7,12))
      )
  
  dev.off()
}


 




###################### for 8ab survey
######## use for loop to plot all families: 


# get unique families
families <- unique(datQ34onlynum$APHIA_FAMILY)

# world map
m <- map_data("worldHires")

## set map boundaries BTS-VIII
stations_BTS_VIII<-stationsQ34%>%filter(Survey=="BTS-VIII")
xmin_8 <- min(stations_BTS_VIII$ShootLong)
xmax_8 <- max(stations_BTS_VIII$ShootLong)
ymin_8 <- min(stations_BTS_VIII$ShootLat)
ymax_8 <- max(stations_BTS_VIII$ShootLat)

# fam <- "Loliginidae"
for (fam in families) {
  
  cat("Processing:", fam, "\n")
  
  ##########################################
  # Filter data
  ##########################################
  dat_fam <- datQ34onlynum %>% 
    filter(APHIA_FAMILY == fam)
  
  ##########################################
  # Summaries
  ##########################################
  dat2 <- dat_fam %>%
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             Valid_Aphia, ShootLat, ShootLong, scientificname,
             APHIA_FAMILY, sweptarea2) %>%
    summarise(n_TotalNo = n_distinct(TotalNo), .groups = "drop")
  
  
  dat3 <- dat2 %>% 
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             ShootLat, ShootLong, APHIA_FAMILY, sweptarea2) %>% 
    summarise(tot = sum(n_TotalNo), .groups = "drop") 
  
  
  ##########################################
  # CPUE
  ##########################################
  dat3 <- dat3 %>% 
    mutate(CPUE = tot / sweptarea2,
           sqrtcpue = sqrt(CPUE))
  
  # head(as.data.frame(dat3))
  # range(dat3$sweptarea2)
  
  # find the max year dynamically
  max_year <- max(dat3$Year, na.rm = TRUE)
  
  # filter the last 10 years
  dat3b <- dat3 %>% 
    filter(CPUE < 1000) %>% 
    filter(Year >= (max_year - 9) & Year <= max_year)
  
  # range(dat3b$Year)
  
  ##########################################
  # MAP (by haul)
  ##########################################
  year_map=c((max(dat$Year)-9):max(dat$Year))
  
  jpeg(file = paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                     fam, "_map8ab.jpg"),
       width = 250, height = 250, units = "mm", res = 300)
  
  print(
    ggplot() +
      theme_light() +
      geom_polygon(data = m, aes(long, lat, group = group),
                   fill = NA, color = "grey") +
      geom_point(data = stationsQ34 %>%
                   filter(Year %in% year_map, Survey == "BTS-VIII"),
                 aes(ShootLong, ShootLat),
                 shape = '.', size = 0.05) +
      geom_point(data = dat3b %>%
                   filter(Year %in% year_map, Survey == "BTS-VIII"),
                 aes(ShootLong, ShootLat, size = CPUE),
                 color = "red", alpha = 0.5) +
      facet_wrap(~Year, dir='h') +
      labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste0(fam, " CPUE by haul"), fill="CPUE (n/km^2)") + 
      coord_quickmap(xlim = c(xmin_8, xmax_8), ylim = c(ymin_8, ymax_8))+
      theme(text=element_text(size=22), axis.text = element_text(size = 14)) +  
      theme(legend.title = element_text(face = "bold", size = 18)) + 
      theme(legend.position="bottom",legend.direction = "horizontal", axis.title = element_text(size = 18)) + 
      theme(legend.text=element_text(face = "italic",size=16)) + 
      guides(colour = guide_legend(override.aes = list(size = 8)))
  )
  
  dev.off()
  
  ##########################################
  # BY SPECIES
  ##########################################
  dat4 <- dat2 %>% 
    group_by(Country, Ship, Survey, Gear, Year, StNo, HaulNo,
             ShootLat, ShootLong, APHIA_FAMILY,
             scientificname, sweptarea2) %>% 
    summarise(tot = sum(n_TotalNo), .groups = "drop") %>%
    mutate(CPUE = tot / sweptarea2,
           sqrtcpue = sqrt(CPUE))
  
  dat4b <- dat4 %>% 
    filter(CPUE < 1000)
  
  
  ##########################################
  # MAP BY SPECIES
  ##########################################
  
  jpeg(file = paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/CEPH/",
                     fam, "_map_species8ab.jpg"),
       width = 350, height = 350, units = "mm", res = 300)
  
  print(
    ggplot() +
      theme_light() +
      geom_polygon(data = m, aes(long, lat, group = group),
                   fill = NA, color = "grey") +
      geom_point(data = dat4b %>%
                   filter(Year %in% year_map, Survey == "BTS-VIII"),
                 aes(ShootLong, ShootLat,
                     shape=scientificname,
                     colour = scientificname,
                     size = CPUE),
                 alpha = 0.5) +
      geom_point(data = stationsQ34 %>%
                   filter(Year %in% year_map, Survey == "BTS-VIII"),
                 aes(ShootLong, ShootLat),
                 shape = '.', size = 0.05) +
      facet_wrap(~Year, dir='h', nrow = 3, ncol = 4) +
      labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", title = paste0(fam," CPUE by haul", fill="CPUE (n/km^2)")) +
      coord_quickmap(xlim = c(xmin_8, xmax_8), ylim = c(ymin_8, ymax_8))+
      theme(text=element_text(size=22), axis.text = element_text(size = 14))+
      theme(legend.title = element_text(face = "bold", size = 18))+ 
      theme(legend.position="bottom",legend.direction = "horizontal", axis.title = element_text(size = 18))+ theme(legend.text=element_text(face = "italic",size=16))+ guides(shape = guide_legend(nrow=4, override.aes = list(size = 8)))+ scale_shape_manual(values=c(15,8,17,16,18,10,7,12))
  )
  
  dev.off()
}




