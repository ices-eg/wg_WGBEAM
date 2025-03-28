#################################### 
# ---
# title: "Indices for brown shrimp and whiting DYFS and SNS"
# date: 27/3/2025
# script by Lies Vansteenbrugge
# ---
#################################### 

#--- Load libraries
library(icesDatras)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
# library(sf)
# library(ggspatial)

#--- Set parameters
y <- 2020
yTo <- 2024

#--- Get data from Datras 
HHDYFS <-getDATRAS(record = "HH",survey = "DYFS",years= y:yTo,quarters = c(3,4))
HHSNS <-getDATRAS(record = "HH",survey = "SNS",years= y:yTo,quarters = c(3,4))

HH <- rbind(HHDYFS, HHSNS)

HLDYFS <-getDATRAS(record = "HL", survey = "DYFS", years= y:yTo, quarters = c(3,4))
HLSNS <- getDATRAS(record = "HL", survey = "DYFS", years= y:yTo, quarters = c(3,4))

HL <- rbind(HLDYFS, HLSNS)

#--- Convert haul duration to hours
table(HH$HaulDur, HH$Survey) # haulduration of 1 min? NL and DE
table(HH$Year, HH$HaulDur)
table(HH$HaulDur, HH$Country)
table(HH$HaulDur, HH$HaulVal)
HH$HaulDur<-as.numeric(HH$HaulDur)/60
# hours<-aggregate(data=HH,HaulDur~Year,sum)
# plot(hours$Year,hours$HaulDur)


#--- Whiting plots
head(HH)
head(HL)
table(HL$SpecCode)

HLwhg <- HL %>% 
  filter(SpecCode == 126438)

head(HLwhg)
table(HLwhg$StNo, useNA = "always")
head(HH)
table(HH$Gear, useNA = "always")
HH$ID <- paste(HH$Year, HH$HaulNo, HH$Country, HH$StNo, HH$Survey, HH$Ship, sep = "_")
HLwhg$ID <- paste(HLwhg$Year, HLwhg$HaulNo, HLwhg$Country, HLwhg$StNo, HLwhg$Survey, HLwhg$Ship, sep = "_")
length(unique(HH$ID))
nrow(HH)
# length(unique(HLwhg$ID))
# nrow(HLwhg)

Merg <- left_join(HH, HLwhg, by = c("ID")) %>%
  filter(HaulVal == "V") %>%
  select(Survey.x, Country.x, Ship.x, StNo.x, HaulNo.x, Year.x, Month, Day, TimeShot, HaulDur, DayNight, ShootLat, ShootLong, HaulLat, HaulLong, Depth, HaulVal, Distance, TotalNo, NoMeas, SubWgt, CatCatchWgt, LngtCode,LngtClass, HLNoAtLngt)%>%
  mutate(HLNoAtLngt = case_when(is.na(HLNoAtLngt) ~ as.integer(0), TRUE ~ HLNoAtLngt)) %>% #hauls without whiting, change the NA's to zero's
  filter(!(HLNoAtLngt == -9)) %>% 
  mutate(TotalNo = case_when(is.na(TotalNo) ~ 0, TRUE ~ TotalNo)) %>%
  filter(!(TotalNo == -9)) %>% 
  filter(!is.na(Year.x)) %>% 
  mutate(NoMeas = case_when(is.na(NoMeas) ~ as.integer(0), TRUE ~ NoMeas)) %>%
  mutate(CatCatchWgt = case_when(is.na(CatCatchWgt) ~ as.integer(0), TRUE ~ CatCatchWgt))

CPUE <- Merg %>%
  select(Survey.x, Country.x, Ship.x, StNo.x, HaulNo.x, Year.x, TotalNo, ShootLat, ShootLong, HaulDur) %>% 
  distinct() %>%
  mutate("CPUE" = TotalNo/HaulDur)

head(CPUE)
table(CPUE$TotalNo, useNA = "always")
range(CPUE$CPUE)

# testio <- CPUE %>% 
#   filter(Country.x == "NL") %>% 
#   filter(Year.x == 2024)
# head(testio)
# table(testio$Ship.x)
# 
# testio <- HH %>% 
#   filter(Country == "NL") %>% 
#   filter(Year == 2022)
# head(testio)
# table(testio$Ship)


my_map <- map_data("worldHires")

# c <- ggplot(my_map) +
#   geom_polygon(aes(long, lat, group=group), fill = "grey55") +
#   geom_point(data = CPUE, aes(x=ShootLong, y=ShootLat), colour = "black", size= 1) +
#   geom_point(data = CPUE, aes(x=ShootLong, y=ShootLat, size= CPUE), colour = "red", alpha = 1) +
#   labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", size="CPUE (numbers/h)") +
#   coord_quickmap(xlim = c(2.5, 9), ylim = c(51, 55.5)) + theme(legend.title=element_text(size=40), legend.text=element_text(size=40), plot.title = element_text(hjust = 0.5, size = 40), axis.text = element_text( size = 40), axis.text.x = element_text( size = 40, ), axis.title = element_text( size = 40),strip.text = element_text(size = 40)) + facet_wrap(~Year.x) + scale_size_continuous(range=c(2,15))+ scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "red"), guide = "none")
# #breaks=c(100, 200, 400, 800, 1600)) 

c <- ggplot(my_map) +
  geom_polygon(aes(long, lat, group=group), fill = "grey55") +
  geom_point(data = subset(CPUE, CPUE == 0), # Plot black points where CPUE = 0
             aes(x = ShootLong, y = ShootLat), colour = "black", size = 2) +
  geom_point(data = subset(CPUE, CPUE > 0), # Plot red points where CPUE > 0
             aes(x = ShootLong, y = ShootLat, size = CPUE), 
             colour = "red", alpha = 1) +
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", size="CPUE (numbers/h)") +
  coord_quickmap(xlim = c(2.5, 9), ylim = c(51, 55.5)) + 
  theme(legend.title = element_text(size = 40), legend.text = element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 40), axis.text = element_text(size = 40), axis.text.x = element_text(size = 40), axis.title = element_text(size = 40), strip.text = element_text(size = 40)) + 
  facet_wrap(~Year.x) + scale_size_continuous(range = c(2, 15))

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEwhg", yTo,".jpg"), plot = c, width = 24, height = 24, units = "in")

# testje <- CPUE %>% 
#   filter(Country.x == "BE") %>% 
#   filter(StNo.x == 16)

head(CPUE)

#Barplots CPUE Totaal
CPUETotal <- CPUE %>%
  group_by(Year.x) %>%
  summarize(tot_dur=sum(HaulDur),tot=sum(TotalNo)) %>%
  mutate(CPUE_year=tot/tot_dur)

head(CPUETotal)


l <- ggplot(CPUETotal)+
  geom_bar(aes(x=as.character(Year.x), y=CPUE_year),stat = "identity")+
  theme(legend.title=element_text(size=30), legend.text=element_text(30), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 30),    axis.text.x = element_text(size = 30, ), axis.title = element_text( size = 30),strip.text = element_text(size = 30)) + labs(x = "Year", y = "Total CPUE (numbers/h)")

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEwhg_bar.jpg"), plot = l, width = 24, height = 24, units = "in")


#Barplots CPUE Totaal ---- by country
CPUETotalcountry <- CPUE %>%
  group_by(Year.x, Country.x, Ship.x, Survey.x) %>%
  summarize(tot_dur=sum(HaulDur),tot=sum(TotalNo)) %>%
  mutate(CPUE_year=tot/tot_dur)

head(CPUETotalcountry)


byc <- ggplot(CPUETotalcountry) +
  geom_bar(aes(x = as.character(Year.x), y = CPUE_year, fill = Country.x), stat = "identity", position = "dodge") +
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30), 
        plot.title = element_text(hjust = 0.5, size = 30), 
        axis.text = element_text(size = 30),    
        axis.text.x = element_text(size = 30), 
        axis.title = element_text(size = 30),
        strip.text = element_text(size = 30)) +
  labs(x = "Year", y = "Total CPUE (numbers/h)", fill = "Country")

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEwhg_bar_bycountry.jpg"), plot = byc, width = 24, height = 24, units = "in")




#--------------------------------------------------------------------------------

#--- Shrimp plot
# Make selections and join all datasets
HLcsh <- HL %>%
  filter(SpecCode == "107552")

head(HLcsh)
table(HLcsh$StNo, useNA = "always")
head(HH)
table(HH$Gear, useNA = "always")
HH$ID <- paste(HH$Year, HH$HaulNo, HH$Country, HH$StNo, HH$Survey, HH$Ship, sep = "_")
HLcsh$ID <- paste(HLcsh$Year, HLcsh$HaulNo, HLcsh$Country, HLcsh$StNo, HLcsh$Survey, HLcsh$Ship, sep = "_")
length(unique(HH$ID))
nrow(HH)


Mergcsh <- left_join(HH, HLcsh, by = c("ID")) %>%
  filter(HaulVal == "V") %>%
  select(Survey.x, Country.x, Ship.x, StNo.x, HaulNo.x, Year.x, Month, Day, TimeShot, HaulDur, DayNight, ShootLat, ShootLong, HaulLat, HaulLong, Depth, HaulVal, Distance, TotalNo, NoMeas, SubWgt, CatCatchWgt, LngtCode,LngtClass, HLNoAtLngt)%>%
  mutate(HLNoAtLngt = case_when(is.na(HLNoAtLngt) ~ as.integer(0), TRUE ~ HLNoAtLngt)) %>% #hauls without whiting, change the NA's to zero's
  filter(!(HLNoAtLngt == -9)) %>% 
  mutate(TotalNo = case_when(is.na(TotalNo) ~ 0, TRUE ~ TotalNo)) %>%
  filter(!(TotalNo == -9)) %>% 
  filter(!is.na(Year.x)) %>% 
  mutate(NoMeas = case_when(is.na(NoMeas) ~ as.integer(0), TRUE ~ NoMeas)) %>%
  mutate(CatCatchWgt = case_when(is.na(CatCatchWgt) ~ as.integer(0), TRUE ~ CatCatchWgt))

head(Mergcsh)

CPUEcsh <- Mergcsh %>%
  select(Survey.x, Country.x, Ship.x, StNo.x, HaulNo.x, Year.x, TotalNo, ShootLat, ShootLong, HaulDur) %>% 
  distinct() %>%
  mutate("CPUE" = TotalNo/HaulDur)

head(CPUEcsh)
table(CPUEcsh$TotalNo, useNA = "always")
table(CPUEcsh$Year.x, useNA = "always")
range(CPUEcsh$CPUE)
summary(CPUEcsh)

my_map <- map_data("worldHires")

cshmap <- ggplot(my_map) +
  geom_polygon(aes(long, lat, group=group), fill = "grey55") +
  geom_point(data = subset(CPUEcsh, CPUE == 0), aes(x=ShootLong, y=ShootLat), colour = "black", size= 2) +
  geom_point(data = subset(CPUEcsh, CPUE >0), aes(x=ShootLong, y=ShootLat, size= CPUE), colour = "red", alpha = 1) +
  labs(x = "Longitude (Degrees)", y = "Latitude (Degrees)", size="CPUE (numbers/h)") +
  coord_quickmap(xlim = c(2.5, 9), ylim = c(51, 55.5)) + 
  theme(legend.title=element_text(size=40), legend.text=element_text(size=40), plot.title = element_text(hjust = 0.5, size = 40), axis.text = element_text( size = 40), axis.text.x = element_text( size = 40, ), axis.title = element_text( size = 40),strip.text = element_text(size = 40)) + 
  facet_wrap(~Year.x) + scale_size_continuous(range=c(2,15))
#breaks=c(100, 200, 400, 800, 1600)) 

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEcsh", yTo,".jpg"), plot = cshmap, width = 24, height = 24, units = "in")

head(CPUEcsh)

#Barplots CPUE Totaal
CPUETotalcsh <- CPUEcsh %>%
  group_by(Year.x) %>%
  summarize(tot_dur = sum(HaulDur),
    tot = sum(TotalNo)) %>% 
  mutate(CPUE_year = tot / tot_dur)

head(CPUETotalcsh)

cshbar <- ggplot(CPUETotalcsh)+
  geom_bar(aes(x=as.character(Year.x), y=CPUE_year),stat = "identity")+
  theme(legend.title=element_text(size=30), legend.text=element_text(30), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 30),    axis.text.x = element_text(size = 30, ), axis.title = element_text( size = 30),strip.text = element_text(size = 30)) + labs(x = "Year", y = "Total CPUE (numbers/h)")

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEcsh_bar.jpg"), plot = cshbar, width = 24, height = 24, units = "in")


#Barplots CPUE Totaal ---- by country
CPUETotalcshcountry <- CPUEcsh %>%
  group_by(Year.x, Country.x, Ship.x, Survey.x) %>%
  summarize(tot_dur=sum(HaulDur),tot=sum(TotalNo)) %>%
  mutate(CPUE_year=tot/tot_dur)

head(CPUETotalcshcountry)


byccsh <- ggplot(CPUETotalcshcountry) +
  geom_bar(aes(x = as.character(Year.x), y = CPUE_year, fill = Country.x), stat = "identity", position = "dodge") +
  theme(legend.title = element_text(size = 30), 
    legend.text = element_text(size = 30), 
    plot.title = element_text(hjust = 0.5, size = 30), 
    axis.text = element_text(size = 30),    
    axis.text.x = element_text(size = 30), 
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 30)) +
  labs(x = "Year", y = "Total CPUE (numbers/h)", fill = "Country")


ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEcsh_bar_bycountry.jpg"), plot = byccsh, width = 24, height = 24, units = "in")



#--------------------------------------------------------------------------------
#################### combine whg and csh plots

head(CPUETotalcsh)
CPUETotalcsh$Species <- "csh"
head(CPUETotal)
CPUETotal$Species <- "whg"

CPUEall <- rbind(CPUETotal, CPUETotalcsh)

ggplot(CPUEall, aes(Year.x, CPUE_year, fill = Species))+
  geom_bar(stat = "identity", position = "dodge")+theme_bw()


# Define a scale factor to adjust the secondary y-axis
scale_factor <- max(CPUETotal$CPUE_year, na.rm = TRUE) / max(CPUETotalcsh$CPUE_year, na.rm = TRUE)

CPUETotalcsh$Species <- "csh"
CPUETotal$CPUE_scaled <- CPUETotal$CPUE_year  # whg values remain the same
CPUETotalcsh$CPUE_scaled <- CPUETotalcsh$CPUE_year * scale_factor  # Scale csh values

CPUEall <- rbind(CPUETotal, CPUETotalcsh)

combi <- ggplot() +
  # First line (whg) on primary y-axis
  geom_line(data = subset(CPUEall, Species == "whg"), 
            aes(x = Year.x, y = CPUE_scaled, color = "whg"), linewidth = 1) +
  
  # Second line (csh) on secondary y-axis (scaled)
  geom_line(data = subset(CPUEall, Species == "csh"), 
            aes(x = Year.x, y = CPUE_scaled, color = "csh"), 
            linewidth = 1, linetype = "dashed") +
  
  scale_y_continuous(name = "CPUE (whg)", 
                     sec.axis = sec_axis(~ . / scale_factor, name = "CPUE (csh)")) +  
  theme_bw() +
  labs(color = "Species")

ggsave(paste0("./ADHOC/OUTPUT/WGBEAM/WGBEAM_2025/allsurveys_CPUEcshwhg_line.jpg"), plot = combi, width = 10, height = 10, units = "in")


