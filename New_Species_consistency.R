###########################################################
#   Temporal new species identification consistency       #
###########################################################

# Install and load missing packages
list.of.packages <- c("icesDatras", "tidyverse","dplyr","tidyr","worms","mapdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(icesDatras)
library(tidyverse)
library(dplyr)
library(tidyr)
library(worms)
library(mapdata)

setwd("C:/Users/XXX/ICES/WGBEAM/script")  ### Please insert here the working directory, or press ctr+shift+h and navigate to the right folder

survey <- "BTS"      # Please select the type of survey you want to analyze
year <- c(2000:2018) # Please select the period in which you want to perform the analysis

## Get station data from Datras 
getSurveyList() # gives overview of all survey acronyms
tmp<-getDATRAS(record = "HH",
               survey = survey,
               years= year,
               quarters = 1:4)

# look what's there
head(tmp)
table(tmp$Year)
table(tmp$Quarter)
table(tmp$Country)

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

## Get fish data from Datras
tmpf<-getDATRAS(record = "HL",
                survey = survey,
                years= year,
                quarters = 1:4)

# look what's there
head(tmpf)
table(tmpf$Year)
table(tmpf$Quarter)
table(tmpf$Country)
table(tmpf$Country, tmpf$Year)

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

## Joining the datasets from Datras and tidy up
dat <-tmpf1 %>%
  left_join(tmp1) 

# look for NAs
for (Var in names(dat)) {
  missing <- sum(is.na(dat[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

head(dat)
table(dat$Year)
table(dat$Quarter)
table(dat$Country)


## Create a species list from the aphia codes
dat$Valid_Aphia <- as.integer(dat$Valid_Aphia)
sp <- worms::wormsbyid(unique(dat$Valid_Aphia))
## Tidy up the species list
sp1<-sp %>% select(Valid_Aphia=AphiaID,scientificname)

## Join species name from worms
dat <- dat %>% 
  left_join(sp1,by="Valid_Aphia")

 
################################################
# check if/which new species enter the database     #
################################################

# comparison between 2017 and 2018 survey #
 dat1 <- dat %>% dplyr::filter(Year %in% c("2017","2018"))
 
 lastYS <- 2018  # put the last survey year

# Find the first year of appearance of the species in the period selected 
first_year_db <-dat1 %>% dplyr::group_by(scientificname) %>%dplyr::summarise(first_year = min(Year));first_year_db 
# find new species in the last available survey
new_species_lastsurvey <-  first_year_db %>% dplyr::filter(first_year == lastYS);new_species_lastsurvey;length(new_species_lastsurvey$scientificname)
write.csv(new_species_lastsurvey,paste(survey, "_new_species_", lastYS, ".csv"))

# Find the first year of appearance of the species by COUNTRY
first_year_db_Country <-dat1 %>% dplyr::group_by(scientificname, Country) %>%dplyr::summarise(first_year = min(Year));first_year_db_Country 
# find new species in the last available survey by COUNTRY
new_species_lastsurvey_Country <-  first_year_db_Country %>% dplyr::filter(first_year == lastYS);new_species_lastsurvey_Country;length(new_species_lastsurvey_Country$scientificname)
write.csv(new_species_lastsurvey_Country,paste(survey, "_new_species_by_Country_", lastYS, ".csv"))
# number of new species by Country
n_by_Country <- new_species_lastsurvey_Country %>% group_by(Country) %>% tally();n_by_Country 

# pie chart new species by COUNTRY 
pct <- round(n_by_Country$n/sum(n_by_Country$n)*100)
lbls <- paste(n_by_Country$Country,"=",n_by_Country$n, "(",pct,"%)") # add percentages to labels
png(paste("Plot_new_species_by_Country.png",sep=""))
pie(n_by_Country$n, labels = lbls, col=rainbow(length(lbls)),main=paste("New species by Country in ",survey,lastYS ))
dev.off()


##########################################
# maps of the new species in last survey #
##########################################

Specie <- "Limecola balthica"  # here select one of the species from "new_species_lastsurvey" db 
plot_db <- dat1 %>% dplyr::filter(scientificname == Specie) %>% dplyr::filter(Year == lastYS)

long1 <- -9  # min Longitude 
long2 <- 10  # max Longitude 
lat1 <-  48 # min Latitude 
lat2 <-  58  # max Latitude 

map1<-map_data("worldHires")
jpeg(paste(Specie,survey,lastYS,".jpeg"),width = 300, height = 150, units = "mm", res = 300)
ggplot(plot_db )+coord_sf(xlim=c(long1,long2),ylim=c(lat1,lat2), expand = T)+geom_polygon(data=map1,aes(long,lat, group=group))+geom_point(alpha=0.65,aes(ShootLong,ShootLat, color=Country),size=3) +theme_light()+theme(legend.title = element_text( size = 15))+ labs(x=NULL,y=NULL, size="", colour =NULL)+ggtitle(paste("New specie: ", Specie), paste(survey, lastYS))
dev.off()


