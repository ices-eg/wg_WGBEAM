library(sf)
library(gridExtra)

dir.create("figures/Cohortplots", showWarnings = FALSE, recursive = TRUE)

## Remove from final data frame missing lat and lons
NO_NA_df_hh_ca <- df_hh_ca %>% filter(is.na(ShootLat) ==FALSE, is.na(ShootLong)==FALSE)
NO_NA_df_hh_hl <- df_hh_hl %>% filter(is.na(ShootLat) ==FALSE, is.na(ShootLong)==FALSE)

dim(df_hh_ca)[1]-dim(NO_NA_df_hh_ca)[1]
dim(df_hh_hl)[1]-dim(NO_NA_df_hh_hl)[1]

##ICES areas from ices website
ices.areas<-st_read("R/ICES_AREA/ICES_Areas_20160601_cut_dense_3857.shp")
## r natural eath map
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

# Ensure CRS is defined (sometimes missing)
st_crs(world) <- 4326

# transform to same crs as world
ices.areas <- st_transform(ices.areas, st_crs(world))

####  convert data frame to spatial points data frame and retain the lat lons, remove argument
Spatial_NO_NA_df_hh_ca <- st_as_sf(NO_NA_df_hh_ca,
                                   coords = c("ShootLong", "ShootLat"),
                                   crs = 4326,
                                   remove =FALSE)

## just the points
spatial_df_hh_ca <- st_as_sf(select(NO_NA_df_hh_ca, ShootLat,ShootLong) %>% unique(),
                             coords = c("ShootLong", "ShootLat"),
                             crs = 4326
)


### intersection on just the points as this is way faaster than running it over the whole data frame
spatial_df_hh_ca_2 <- st_intersection(spatial_df_hh_ca,ices.areas)

## this allows us to join back on to the dataframe
dim(Spatial_NO_NA_df_hh_ca )
spatial_df_hh_ca_3 <- st_join(Spatial_NO_NA_df_hh_ca,spatial_df_hh_ca_2 ,by = geometry)
dim(Spatial_NO_NA_df_hh_ca )[1]-dim(spatial_df_hh_ca_3)[1]
#############
# spatial_df_hh_hl <- st_as_sf(select(NO_NA_df_hh_hl, ShootLat,ShootLong) %>% unique(),
#                              coords = c("ShootLong", "ShootLat"),
#                              crs = 4326)
# spatial_df_hh_hl_2 <- st_intersection(spatial_df_hh_hl,ices.areas)
#

#Regions
#North sea 4.a,b,c, 7.d, 3.a
##Irish  7.a
##Celtic  7.e,f,g,h,j,k

##### used an ifelse for in norht sea and not in north sea
spatial_df_hh_ca_3 <- spatial_df_hh_ca_3 %>% mutate(SurveySeries = ifelse(Area_27 %in% c("4.a","4.b","4.c","3.a.20","7.d"),"BTS North Sea","BTS Celtic Sea & Irish Sea"))

spatial_df_hh_ca_3_CEL_IRISH <- spatial_df_hh_ca_3 %>% filter(SurveySeries %in% c("BTS Celtic Sea & Irish Sea")) %>% mutate(EastWest="BTS Celtic Sea & Irish Sea")
### east west divide
# separate west and east data and only select area inside 51N-57.5N
spatial_df_hh_ca_3_NORTHSEA <-spatial_df_hh_ca_3 %>%  filter(SurveySeries %in% c("BTS North Sea")) %>% mutate(checkKey=1:length(RecordType))

#west compnent
dat_w <- spatial_df_hh_ca_3_NORTHSEA %>%
  filter(ShootLong > -2 & ShootLong < 3 & ShootLat >= 51 & ShootLat <= 57.5) %>% mutate(EastWest="BTS Western North Sea")

#east component
dat_e <- spatial_df_hh_ca_3_NORTHSEA  %>%  filter(ShootLong >= 3 & ShootLat >= 51 & ShootLat <= 57.5) %>% mutate(EastWest="BTS Eastern North Sea")

#### 7d 
dat_7d <- spatial_df_hh_ca_3_NORTHSEA  %>%  filter(Area_27 %in% c("7.d")) %>% mutate(EastWest="BTS 7D North Sea")

#bind
dat_ew <- rbind(dat_w,dat_e)
# use the check key for everything not inside 51, 57
dat_out <- spatial_df_hh_ca_3_NORTHSEA %>% filter(!checkKey %in% dat_ew$checkKey) %>% mutate(EastWest="BTS NorthS Sea outside 51N-57N")


###### bind back together  and remvoe check key 
spatial_df_hh_ca_3_NORTHSEA_2 <- rbind(dat_ew,dat_out) 

spatial_df_hh_ca_3_NORTHSEA_2 <- rbind(spatial_df_hh_ca_3_NORTHSEA_2,dat_7d) %>% select(-checkKey)

#### stick parts back together chck dims
spatial_df_hh_ca_4 <- rbind(spatial_df_hh_ca_3_NORTHSEA_2,spatial_df_hh_ca_3_CEL_IRISH)
dim(spatial_df_hh_ca_3)[1]-dim(spatial_df_hh_ca_4)[1]


###### important uncount the ca table so each row = 1 fish this allows the geomhistorgram to function as inteded
spatial_df_hh_ca_5 <- spatial_df_hh_ca_4 %>% uncount(weights = CANoAtLngt)
sum(spatial_df_hh_ca_4$CANoAtLngt)-dim(spatial_df_hh_ca_5)[1] # if not zero problem


plot_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName =="Pleuronectes platessa")

library(ggFishPlots)

for(i in unique(spatial_df_hh_ca_5$ScientificName)){

plot_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName ==i)

plot_names <- c()

if("NL" %in% plot_dat$Country){
pout_NL <- plot_lw(filter(plot_dat, Country %in% c("NL")),length ="LngtClass",weight ="IndWgt",
                     # ylab = "Weight (g)",
                     # xlab = "Length Class (cm)",
                     length.unit = "cm",
                     weight.unit = "g",
                     annotate.coefficients = T
                    )
##add tittle and ge tonly plot 
p1 <- pout_NL$plot+ggtitle(paste("Length weight plot of ",i, " for NL",sep=""))+theme_gray()
plot_names <- c(plot_names,"p1")
}

if("BE" %in% plot_dat$Country){
pout_BE <- plot_lw(filter(plot_dat, Country %in% c("BE")),length ="LngtClass",weight ="IndWgt",
                   # ylab = "Weight (g)",
                   # xlab = "Length Class (cm)",
                   length.unit = "cm",
                   weight.unit = "g",
                   annotate.coefficients = T
)
##add tittle and ge tonly plot 
p2 <-pout_BE$plot+ggtitle(paste("Length weight plot of ",i, " for BE",sep=""))+theme_gray()
plot_names <- c(plot_names,"p2")
}

if("GB" %in% plot_dat$Country){
pout_GB <- plot_lw(filter(plot_dat, Country %in% c("GB")),length ="LngtClass",weight ="IndWgt",
                   # ylab = "Weight (g)",
                   # xlab = "Length Class (cm)",
                   length.unit = "cm",
                   weight.unit = "g",
                   annotate.coefficients = T
)
##add tittle and ge tonly plot 
p3 <-pout_GB$plot+ggtitle(paste("Length weight plot of ",i, " for GB",sep=""))+theme_gray()
plot_names <- c(plot_names,"p3")
}
if("DE" %in% plot_dat$Country){
pout_DE <- plot_lw(filter(plot_dat, Country %in% c("DE")),length ="LngtClass",weight ="IndWgt",
                   # ylab = "Weight (g)",
                   # xlab = "Length Class (cm)",
                   length.unit = "cm",
                   weight.unit = "g",
                   annotate.coefficients = T
)
##add tittle and ge tonly plot 
p4 <-pout_DE$plot+ggtitle(paste("Length weight plot of ",i, " for DE",sep=""))+theme_gray()
plot_names <- c(plot_names,"p4")
}


grobs <- mget(
  plot_names[plot_names %in% ls(envir = .GlobalEnv)],
  envir = .GlobalEnv
)



ggsave(plot = 
grid.arrange(nrows=4,ncol=1,grobs = grobs),
paste("figures/LW_PLOT/LW_plot_cou_",i,".png",sep=""),width = 12, height = 12, dpi = 300
)

}

######## ggplot version
for(i in unique(spatial_df_hh_ca_5$ScientificName)){
  
plot_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName ==i)
  
ggplot(plot_dat)+geom_point(aes(x=LngtClass, y=IndWgt,colour = Country),alpha = 3/10)+
  labs( x = "Length Class (cm)", y= "Weight (g)")+ggtitle(paste("Length weight plot for ",i,sep=""))+
  scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
  )+
  scale_y_continuous(breaks = seq(0, max(plot_dat$IndWgt, na.rm = T)+10, by = 100)
  )+theme_grey()
  
  ggsave(paste("figures/LW_PLOT/LW_Plot_",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
}

######## now by plotting area same as cohort plots 

for(i in unique(spatial_df_hh_ca_5$ScientificName)){
  
  ##### with NAs
  Species_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName == i)
  
  ### we need to have the whole BTS north sea plus eas and west so we split apply and combine (good old hadly)
  plot_dat_1 <- Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS NorthS Sea outside 51N-57N","BTS 7D North Sea")) %>% 
    mutate(EastWest ="BTS North Sea")
  
  plot_dat_2 <-Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS 7D North Sea"))
  
  plot_dat_3 <-Species_dat %>% filter(EastWest %in% c("BTS Celtic Sea & Irish Sea"))
  
  # put back together
  plot_dat <- rbind(plot_dat_1,plot_dat_2)
  plot_dat <- rbind(plot_dat,plot_dat_3)
  
  ggplot(plot_dat)+geom_point(aes(x=LngtClass, y=IndWgt,colour = EastWest),alpha = 3/10)+
    labs(colour="Area" ,x = "Length Class (cm)", y= "Weight (g)")+ggtitle(paste("Length weight plot for ",i,sep=""))+
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
    )+
    scale_y_continuous(breaks = seq(0, max(plot_dat$IndWgt, na.rm = T)+10, by = 100)
    )+theme_grey()
  
  ggsave(paste("figures/LW_PLOT/LW_Plot_Area_",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
}


#### with a facet 

######## now by plotting area same as cohort plots 

for(i in unique(spatial_df_hh_ca_5$ScientificName)){
  
  ##### with NAs
  Species_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName == i)
  
  ### we need to have the whole BTS north sea plus eas and west so we split apply and combine (good old hadly)
  plot_dat_1 <- Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS NorthS Sea outside 51N-57N","BTS 7D North Sea")) %>% 
    mutate(EastWest ="BTS North Sea")
  
  plot_dat_2 <-Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS 7D North Sea"))
  
  plot_dat_3 <-Species_dat %>% filter(EastWest %in% c("BTS Celtic Sea & Irish Sea"))
  
  # put back together
  plot_dat <- rbind(plot_dat_1,plot_dat_2)
  plot_dat <- rbind(plot_dat,plot_dat_3)
  
  ggplot(plot_dat)+geom_point(aes(x=LngtClass, y=IndWgt,colour = EastWest),alpha = 3/10)+
    labs(colour="Area" ,x = "Length Class (cm)", y= "Weight (g)")+ggtitle(paste("Length weight plot for ",i,sep=""))+
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
    )+
    scale_y_continuous(breaks = seq(0, max(plot_dat$IndWgt, na.rm = T)+10, by = 100)
    )+theme_grey()+facet_wrap(~EastWest)
  
  ggsave(paste("figures/LW_PLOT/LW_Plot_Area_facet_",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
}




