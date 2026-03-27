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
dat_out <- spatial_df_hh_ca_3_NORTHSEA %>% filter(!checkKey %in% c(dat_ew$checkKey,dat_7d$checkKey)) %>% mutate(EastWest="BTS NorthS Sea outside 51N-57N and 7D")


###### bind back together  and remvoe check key 
spatial_df_hh_ca_3_NORTHSEA_2 <- rbind(dat_ew,dat_out) 

spatial_df_hh_ca_3_NORTHSEA_2 <- rbind(spatial_df_hh_ca_3_NORTHSEA_2,dat_7d) %>% select(-checkKey)

#### stick parts back together chck dims
spatial_df_hh_ca_4 <- rbind(spatial_df_hh_ca_3_NORTHSEA_2,spatial_df_hh_ca_3_CEL_IRISH)

####################################################
##############THIS CHECK IS REALLY IMPORTANT!#######
dim(spatial_df_hh_ca_3)[1]-dim(spatial_df_hh_ca_4)[1]
#####################################################

###### important uncount the ca table so each row = 1 fish this allows the geomhistorgram to function as inteded
spatial_df_hh_ca_5 <- spatial_df_hh_ca_4 %>% uncount(weights = CANoAtLngt)
sum(spatial_df_hh_ca_4$CANoAtLngt)-dim(spatial_df_hh_ca_5)[1] # if not zero problem

#####################################

library(ggridges)


for(i in unique(spatial_df_hh_ca_5$ScientificName)){
  
  ##### with NAs
  Species_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName == i)
  
  ### we need to have the whole BTS north sea plus eas and west so we split apply and combine (good old hadly)
  # plot_dat_1 <- Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS NorthS Sea outside 51N-57N","BTS 7D North Sea")) %>% 
  #   mutate(EastWest ="BTS North Sea")
  # 
  # plot_dat_2 <-Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS 7D North Sea"))
  # 
  # plot_dat_3 <-Species_dat %>% filter(EastWest %in% c("BTS Celtic Sea & Irish Sea"))
  
  # # put back together
  # plot_dat <- rbind(plot_dat_1,plot_dat_2)
  # plot_dat <- rbind(plot_dat,plot_dat_3)
  
  plot_dat <- Species_dat %>%  filter(SurveySeries %in% c("BTS North Sea"))
  

    ggplot(plot_dat,aes(x=LngtClass,y=Year,group = Year,fill = as.factor(Year),alpha=0.3))+
      geom_density_ridges()+
      scale_alpha(guide = "none")+
      labs(fill="Year" ,x = "Length Class (cm)", y= "Year")+
      ggtitle(paste("Density ridge plot for ",i,sep=""))+
      scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
      )+ 
      scale_y_continuous(breaks = sort(unique(plot_dat$Year)))+
      theme_grey()
  
  ggsave(paste("figures/Den_PLOT/Den_Plot_NS",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  

  ggplot(plot_dat,aes(x=LngtClass,y=Year,group = Year,fill = 0.5 - abs(0.5 - after_stat(ecdf))))+
    geom_density_ridges_gradient( calc_ecdf = TRUE)+
    scale_fill_viridis_c(name = "Tail probability", direction = -1)+
    labs(fill="Year" ,x = "Length Class (cm)", y= "Year")+
    ggtitle(paste("Density ridge plot for ",i," ridges show ecdf probability of individuals",sep=""))+
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
    )+ 
    scale_y_continuous(breaks = sort(unique(plot_dat$Year)))
    theme_grey()
  
  
  ggsave(paste("figures/Den_PLOT/Den_Plot_NS_ECD",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
  
  
  ggplot(plot_dat,aes(x=LngtClass,y=Country,group = Country,fill = as.factor(Year),alpha=0.3))+
    geom_density_ridges()+
    scale_alpha(guide = "none")+
    labs(fill="Year" ,x = "Length Class (cm)", y= "Year")+
    ggtitle(paste("Density ridge plot for ",i,sep=""))+
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
    )+
    facet_wrap(~Year)+
    theme_grey()
  
  ggsave(paste("figures/Den_PLOT/Den_Plot_NS_Cou",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
  
}


for(i in unique(spatial_df_hh_ca_5$ScientificName)){
  
  ##### with NAs
  Species_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName == i)
  
  
  plot_dat <- Species_dat %>%  filter(SurveySeries %in% c("BTS North Sea"))
  plot_dat$Age <- as.factor(plot_dat$Age)
    
  ggplot(plot_dat,aes(x=LngtClass,y=Age,group = Age,fill = as.factor(Age),alpha=0.3))+
    geom_density_ridges()+
    scale_alpha(guide = "none")+
    labs(fill="Age" ,x = "Length Class (cm)", y= "Age")+
    ggtitle(paste("Density ridge plot of age for ",i,sep=""))+
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
    )+
    facet_wrap(~Year)+
    theme_grey()
  
  ggsave(paste("figures/Den_Age_PLOT/Den_Plot_NS_Age",i,".png",sep=""),width = 12, height = 12, dpi = 300)
  
  plot_dat$Country <- as.factor(plot_dat$Country)
  
  # ggplot(plot_dat,
  #        aes(x = LngtClass,
  #            y = factor(Age),
  #            group = interaction(Age, Country)
  #            )) +
  #   geom_density_ridges(aes(fill = Country),alpha = 0.35, scale = 1) +
  #   labs(fill = "Country", x = "Length Class (cm)", y = "Age") +
  #   ggtitle(paste0("Density ridge plot by country and age for ", i)) +
  #   scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = TRUE) + 1, by = 2)) +
  #   facet_wrap(~Year) +
  #   theme_grey()
  
  
  
}

