# Jip Vrooman, 20-03-2024
# Script developed during WGBEAM 2024
# for further checking species inconsistency in the DYFS data
# Number of IDs checked throughout the years, and some more detail for some species as 
# identified in the overall table. (For script for overall table: see script by Holger ('species_consistency_check_DYFS.R'))

install.packages("icesDatras")
install.packages("worrms")


library(icesDatras)
library(ggplot2)
library(RColorBrewer)
library(worrms)

# ---- 0. Read data from DATRAS ----

#Get HL catch data from Datras
HL <- getDATRAS(record = "HL", survey = "DYFS", years = 2000:2023, quarters = 1:4)
head(HL)
str(HL$Valid_Aphia)

save(HL, file = "//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/HL_all.csv")
load("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/HL_all.csv")

#Calculate number caught based on subfactor
HL$caught <- HL$SubFactor*HL$HLNoAtLngt
#In case it's negative, use TotalNo
HL$caught[HL$caught <0] <- HL$TotalNo[HL$caught <0]

# ---- 1. Number of species IDs throughout the years ----
head(HL)

#Add unique haulnr
HL$HaulID <- apply( HL[ , c(2:6,8,10,11,12) ] , 1 , paste , collapse = "" )

#Hauls per year per country (for control)
Hauls_year <- aggregate(data=HL, HaulID~Year+Country, function(x) length(unique(x)))
head(Hauls_year)

#Unique ID's per year per country
IDs_year <- aggregate(data=HL, Valid_Aphia~Year+Country, function(x) length(unique(x)))
head(IDs_year)

#Plot Hauls
p<-ggplot(data=Hauls_year, aes(x=Year, y=HaulID, fill = Country)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Number of hauls") +
  labs(fill = "Country") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Country, nrow = 3)
p

#Plot IDs
p<-ggplot(data=IDs_year, aes(x=Year, y=Valid_Aphia, fill = Country)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Number of unique ID's") +
  labs(fill = "Country") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Country, nrow = 3)
p


# ---- 2. Lemon sole ----

#Select lemon sole
HL_lemsol <- HL[HL$Valid_Aphia == 127140,]

#total number per year per country
coun_year_sum <- aggregate(HL_lemsol$caught, by=list(HL_lemsol$Year, HL_lemsol$Country), FUN=sum, simplify = TRUE, drop = TRUE)
head(coun_year_sum)

#Plot
p<-ggplot(data=coun_year_sum, aes(x=Group.1, y=x, fill = Group.2)) +
  geom_bar(stat="identity", position="dodge") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Country") +scale_fill_brewer(palette = "Set1")
p

#Save figure
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/lemonsole_over_years.png", width=100, height = 50 ,units='mm', res = 300)
p
dev.off()

# ---- 3. Seahorse ----

#Select seahorse species
Seahorse <- HL[HL$Valid_Aphia %in% c(127380,1525460,154776),]

#Sum over years
sum_year <- aggregate(Seahorse$caught, by=list(Seahorse$Year , Seahorse$Country, Seahorse$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Plot Seahorse
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
#theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Seahorse_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()

# ---- 4. Ammodytus ----

#Select Ammod
Ammod <- HL[HL$Valid_Aphia %in% c(125909, 126751,126752),]
head(Ammod)

#Sum over years
sum_year <- aggregate(Ammod$caught, by=list(Ammod$Year , Ammod$Country, Ammod$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Plot Ammod
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
  #theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Ammod_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()

# ---- 5. Syngnatus ----

#Select Sygna
Sygna <- HL[HL$Valid_Aphia %in% c(10327,
                                  126227,
                                  127387,
                                  127389,
                                  127393),]
head(Sygna)

#Sum over years
sum_year <- aggregate(Sygna$caught, by=list(Sygna$Year , Sygna$Country, Sygna$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Plot Sygna
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
  #theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Sygna_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()

# ---- 6. Pomatochistis ----

#Select Poma
Poma <- HL[HL$Valid_Aphia %in% c(125537,
                                  125999,
                                  126925,
                                  126927,
                                  126928,
                                  126930),]
head(Poma)

#Sum over years
sum_year <- aggregate(Poma$caught, by=list(Poma$Year , Poma$Country, Poma$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Plot Poma
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
#theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Poma_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()

# ---- 7. Loligo ----

#Select Loligo
Loligo <- HL[HL$Valid_Aphia %in% c(11734,
                                   138139,
                                   140270,
                                   140271),]
head(Loligo)

#Sum over years
sum_year <- aggregate(Loligo$caught, by=list(Loligo$Year , Loligo$Country, Loligo$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Remove 2002 for NL
#sum_year <- sum_year[!(sum_year$Group.1 ==2002 & sum_year$Group.2 == "NL"),]

#Plot Loligo
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
#theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Loligo_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()

# ---- 8. Liparis ----

#Select Lipar
Lipar <- HL[HL$Valid_Aphia %in% c(126160,
                                  127219,
                                  293624,
                                  127220),]
head(Lipar)

#Sum over years
sum_year <- aggregate(Lipar$caught, by=list(Lipar$Year , Lipar$Country, Lipar$Valid_Aphia), FUN=sum, simplify = TRUE, drop = TRUE)
head(sum_year)

#Add Scientific name
for (i in (1:nrow(sum_year))) {
  sum_year$ScienName[i] <- wm_id2name(sum_year$Group.3[i])
} 

#Remove 2002 for NL
#sum_year <- sum_year[!(sum_year$Group.1 ==2002 & sum_year$Group.2 == "NL"),]

#Plot Lipar
p<-ggplot(data=sum_year, aes(x=Group.1, y=x, fill = ScienName)) +
  geom_bar(stat="identity") +xlab("Year") + ylab("Total number in survey") +
  labs(fill = "Spec") +scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ Group.2, nrow = 3, scales = "free_y") #+
#theme(legend.position="bottom")
p

#Save
png("//WURNET.NL/Homes/vroom009/AppData/FolderRedirection/Desktop/WGBEAM 2024/Lipar_over_years.png", width=200, height = 150 ,units='mm', res = 300)
p
dev.off()
