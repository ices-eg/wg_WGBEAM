########################################################################
#Script to test Species consistency of BTS and DYFS
########################################################################


library(dplyr)
library(icesDatras)
library(worrms)

########################
#BTS
########################
d <- getDATRAS(record="HL", "BTS",years=2025,quarters=c(3))

#save d
#save(d,file="d:/WGBEAM2024/DYFS_species.RData")

###total number of AphiaIDs
length(unique(d$Valid_Aphia))

####number of species per Country
NL <- subset(d, d$Country=="NL")
length(unique(NL$Valid_Aphia))
BE <- subset(d, d$Country=="BE")
length(unique(BE$Valid_Aphia))
DE <- subset(d, d$Country=="DE")
length(unique(DE$Valid_Aphia))

####conect Aphia_IDs to WORMS (using worrms)
specs <- unique(d$Valid_Aphia)
t <- c()
for(i in 1:length(specs)){
  t1 <- wm_record(specs[i])
  t <- rbind(t, t1)}

unac <- subset(t, t$status=="unaccepted")
unique(unac$AphiaID)
unique(unac$scientificname)
unique(unac$valid_AphiaID)
unique(unac$valid_name)

dubio <- subset(t, t$status=="nomen dubium")

######create look up table to check occurence of species per country
d$count <- d$SubFactor*d$HLNoAtLngt
d$count[d$HLNoAtLngt==-9] <- d$TotalNo[d$HLNoAtLngt==-9]

dat1 <- aggregate(count~Valid_Aphia+Country,data=d,FUN="sum")
dat1$species <- wm_id2name_(id=dat1$Valid_Aphia)

dat1BE <- subset(dat1, dat1$Country=="BE")
colnames(dat1BE)[3] <- "BE"
dat1BE$Country <- NULL


dat1NL <- subset(dat1, dat1$Country=="NL")
colnames(dat1NL)[3] <- "NL"
dat1NL$Country <- NULL

dat1DE <- subset(dat1, dat1$Country=="DE")
colnames(dat1DE)[3] <- "DE"
dat1DE$Country <- NULL

ltable <- merge(dat1BE,dat1NL, all=T)
ltable <- merge(ltable, dat1DE, all=T)

ltable[ltable<=0] <- -9

ltable$species <- as.character(ltable$species)
ltable <- ltable[order(ltable$species),]

ltable_BTS <- ltable%>%
  rename(BTS_BE = BE, 
         BTS_NL = NL,
         BTS_DE = DE)

#####save ltable
direct <- "C:/Users/bvertongen/Documents/Development/Rstudio/D1SCI/VISBIO/NDGP/ndgp.ices.wgbeam.datras/ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/SpeciesConsist/"


write.table(ltable, paste0(direct,"WGBEAM26_SpecDiv_BTS.csv"), sep=",", row.names=F)


##############################
#DYFS
##############################
d <- getDATRAS(record="HL", "DYFS",years=2025,quarters=c(3,4))

#save d
#save(d,file="d:/WGBEAM2024/DYFS_species.RData")

###total number of AphiaIDs
length(unique(d$Valid_Aphia))

####number of species per Country
NL <- subset(d, d$Country=="NL")
length(unique(NL$Valid_Aphia))
BE <- subset(d, d$Country=="BE")
gob <- BE%>%
  filter(SpecCode%in%125537)%>%
  select(SpecCode,
         StNo, HaulNo)%>%
  distinct(StNo, .keep_all = T)


length(unique(BE$Valid_Aphia))
DE <- subset(d, d$Country=="DE")
length(unique(DE$Valid_Aphia))

####conect Aphia_IDs to WORMS (using worrms)
specs <- unique(d$Valid_Aphia)
t <- c()
for(i in 1:length(specs)){
  t1 <- wm_record(specs[i])
  t <- rbind(t, t1)}

unac <- subset(t, t$status=="unaccepted")
unique(unac$AphiaID)
unique(unac$scientificname)
unique(unac$valid_AphiaID)
unique(unac$valid_name)

dubio <- subset(t, t$status=="nomen dubium")

######create look up table to check occurence of species per country
d$count <- d$SubFactor*d$HLNoAtLngt
d$count[d$HLNoAtLngt==-9] <- d$TotalNo[d$HLNoAtLngt==-9]

dat1 <- aggregate(count~Valid_Aphia+Country,data=d,FUN="sum")
dat1$species <- wm_id2name_(id=dat1$Valid_Aphia)

dat1BE <- subset(dat1, dat1$Country=="BE")
colnames(dat1BE)[3] <- "BE"
dat1BE$Country <- NULL


dat1NL <- subset(dat1, dat1$Country=="NL")
colnames(dat1NL)[3] <- "NL"
dat1NL$Country <- NULL

dat1DE <- subset(dat1, dat1$Country=="DE")
colnames(dat1DE)[3] <- "DE"
dat1DE$Country <- NULL

ltable <- merge(dat1BE,dat1NL, all=T)
ltable <- merge(ltable, dat1DE, all=T)

ltable[ltable<=0] <- -9

ltable$species <- as.character(ltable$species)
ltable <- ltable[order(ltable$species),]

ltable_DYFS  <- ltable%>%
  rename(DYFS_BE = BE, 
         DYFS_NL = NL,
         DYFS_DE = DE)


#####save ltable
direct <- "C:/Users/bvertongen/Documents/Development/Rstudio/D1SCI/VISBIO/NDGP/ndgp.ices.wgbeam.datras/ADHOC/OUTPUT/WGBEAM/WGBEAM_2026/SpeciesConsist/"


write.table(ltable, paste0(direct,"WGBEAM26_SpecDiv_BTS.csv"), sep=",", row.names=F)

#########################################
#merge tables-----
#########################################

table_combined <- full_join(ltable_BTS,ltable_DYFS, 
                            by = "species")%>%
  select(!Valid.Aphia.y)

write.table(table_combined, paste0(direct,"WGBEAM26_SpecDiv_BTS_DYFS.csv"), sep=",", row.names=F)


