########################################################################
#   Check litter consistency accross offshore beam trawl surveys 
#   WGBEAM Meeting 2022
########################################################################
# Install and load packages
library(icesDatras)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Set years for which you want to request data
years <- c(2019:2021)

LT_ALL <- c()

for (i in seq_along(years)) {
  test <- getLTassessment("BTS", years[i], 3) %>%
    select(Survey, Quarter, Year, Ship, Gear, Country, StNo, HaulNo, ShootLat, ShootLong, HaulLat, HaulLong,
           BottomDepth, Distance, LTREF, PARAM, LTSZC, UnitWgt, LT_Weight, UnitItem, LT_Items, LTSRC,
           TYPPL, LTPRP, Month, Day, TimeShot, HaulDur, StatRec, Depth, HaulVal) %>%
    mutate(LTSRC = as.character(LTSRC)) 
    
  LT_ALL <- bind_rows(test, LT_ALL)
  }


# Create Material parameter
LT_ALL <- LT_ALL %>%
  mutate("Material" = case_when(PARAM == "LT-TOT" ~ "NA", TRUE ~ substr(PARAM, 1,1)))

#Make stacked charts which material is collected per country
ggplot(LT_ALL, aes(fill=Material, x=Country)) + 
  labs(title = "Comparison of litter classes BTS Survey") +
  geom_bar(position="fill", stat="count") + scale_fill_discrete(labels = c("Plastic", "Metal", "Rubber", "Glass/ceramics", "Natural products", "Miscellaneous", "No litter collected")) +
  facet_wrap(~Year) + theme(legend.title=element_text(size=30), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20)) +
  ylab("Ratio")

#Make stacked charts which class of plastic/rubber is collected per country
Zoom <- LT_ALL %>%
  filter(Material == "A" | Material == "C")

ggplot(Zoom, aes(fill=PARAM, x=Country)) + 
  labs(title = "Comparison of litter classes Plastic & Rubber BTS Survey") +
  geom_bar(position="fill", stat="count") + scale_fill_discrete(name = "Class", labels = c("A1: Bottle", "A10: Strapping band", "A11: Crates/Containers", "A12: Diapers", "A13: Towels/Tampons", "A14: Other", "A2: Sheet", "A3: Bag", "A4: Caps/lids", "A5: Monofilament", "A6: Entangled monofilament", "A7: Synthetic rope", "A8: Fishing net", "A9: Cable Ties", 
                                                                           "C1: Boots", "C2: Balloons", "C3: Bobbins (fishing)", "C4: Tyre", "C5: Glove", "C6: Other")) +
  facet_wrap(~Year) + theme(legend.title=element_text(size=24), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20)) +
  ylab("Ratio")

#Monofilament (what are sizes). Should be longest length multiplied with thickness 
#(for fishing monofilaments: 1mm --> length should be longer then 2,5 m (B:F unlikely))
ggplot(LT_ALL[LT_ALL$PARAM == "A5",], aes(fill=LTSZC, x=Year)) + 
  geom_bar(position="fill", stat="count") + ylab("Ratio") + scale_fill_discrete(name = "Size Class") +
  facet_wrap(~Country) + theme(legend.title=element_text(size=24), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20))
