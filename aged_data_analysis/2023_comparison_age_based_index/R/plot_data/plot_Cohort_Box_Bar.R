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
#bind
dat_ew <- rbind(dat_w,dat_e)
# use the check key for everything not inside 51, 57
dat_out <- spatial_df_hh_ca_3_NORTHSEA %>% filter(!checkKey %in% dat_ew$checkKey) %>% mutate(EastWest="BTS NorthS Sea outside 51N-57N")

###### bind back together  and remvoe check key
spatial_df_hh_ca_3_NORTHSEA_2 <- rbind(dat_ew,dat_out) %>% select(-checkKey)

#### stick parts back together chck dims
spatial_df_hh_ca_4 <- rbind(spatial_df_hh_ca_3_NORTHSEA_2,spatial_df_hh_ca_3_CEL_IRISH)
dim(spatial_df_hh_ca_3)[1]-dim(spatial_df_hh_ca_4)[1]


###### important uncount the ca table so each row = 1 fish this allows the geomhistorgram to function as inteded
spatial_df_hh_ca_5 <- spatial_df_hh_ca_4 %>% uncount(weights = CANoAtLngt)
sum(spatial_df_hh_ca_4$CANoAtLngt)-dim(spatial_df_hh_ca_5)[1] # if not zero problem

############# now to test some of the graphs
spatial_df_hh_ca_5
### we need to have the whole BTS north sea plus eas and west so we split apply and combine (good old hadly)
plot_dat_1 <- spatial_df_hh_ca_5 %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS NorthS Sea outside 51N-57N")) %>%
  mutate(EastWest ="BTS North Sea")

plot_dat_2 <-spatial_df_hh_ca_5 %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea"))

plot_dat_3 <-spatial_df_hh_ca_5 %>% filter(EastWest %in% c("BTS Celtic Sea & Irish Sea"))

# put back together
plot_dat <- rbind(plot_dat_1,plot_dat_2)
plot_dat <- rbind(plot_dat,plot_dat_3)


######################################################
#### yes i have left the NA in the plot for now....
#####################################################

### plot can then be facet wrapped by correct eastwest (not yet split by species!)
p1 <- ggplot(plot_dat, aes(LngtClass, fill = factor(Age), color = factor(Age))) +
  geom_histogram(alpha = 0.5,  binwidth = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(spatial_df_hh_ca_5$LngtClass, na.rm = T), max(spatial_df_hh_ca_5$LngtClass, na.rm = T), by = 2), limits = c(0,max(spatial_df_hh_ca_5$LngtClass))) +
  labs(x = "Fish length (cm)", fill = "Age", color = "Age")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(EastWest~.,scale="free_y",nrow=1)
  #+ggtitle(plot_dat$ScientificName)

### box plot  (not yet split by species!)

p2 <- ggplot(plot_dat, aes(x = Age, y = LngtClass, fill = factor(Age),group = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs( y = "Length Class (cm)", fill = "Age") +
  #facet_wrap(~Year) +
  scale_x_discrete(limits = levels(factor(plot_dat$Age)))+
  scale_y_continuous(
    limits = c(0, 60),  # Specify your desired limits
    breaks = seq(0, 60, 10)
  )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(EastWest~.,scale="free_y",nrow=1)

##### check plot arrangement

grid.arrange(nrow=2,p2,p1)


### plot out the cohorts

### fear not the loop for it is a friend
for(i in unique(spatial_df_hh_ca_5$ScientificName)){

  Species_dat <- spatial_df_hh_ca_5 %>% filter(ScientificName == i, !is.na(Age))

  ### we need to have the whole BTS north sea plus eas and west so we split apply and combine (good old hadly)
  plot_dat_1 <- Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea","BTS NorthS Sea outside 51N-57N")) %>%
    mutate(EastWest ="BTS North Sea")

  plot_dat_2 <-Species_dat %>% filter(EastWest %in% c("BTS Western North Sea","BTS Eastern North Sea"))

  plot_dat_3 <-Species_dat %>% filter(EastWest %in% c("BTS Celtic Sea & Irish Sea"))

  # put back together
  plot_dat <- rbind(plot_dat_1,plot_dat_2)
  plot_dat <- rbind(plot_dat,plot_dat_3)

  if(i=="Limanda limanda"){
    plot_dat <- plot_dat %>% filter(!EastWest %in% c("BTS Celtic Sea & Irish Sea"))
  }

  plot_dat$LngtClass <- round(plot_dat$LngtClass,0)

  ### plot can then be facet wrapped by correct eastwest (not yet split by species!)
  p1 <- ggplot(plot_dat, aes(LngtClass, fill = factor(Age), color = factor(Age))) +
    geom_histogram(alpha = 0.5,  binwidth = 1) +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, max(plot_dat$LngtClass, na.rm = T)+1, by = 2)
                       ) +
    labs(x = "Fish length (cm)", fill = "Age", color = "Age")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_wrap(EastWest~.,scale="free_y",nrow=1)+
    ggtitle(plot_dat$ScientificName)

  ### box plot  (not yet split by species!)

  p2 <- ggplot(plot_dat, aes(x = as.numeric(Age), y = LngtClass, fill = factor(Age),group = Age)) +
    geom_boxplot() +
    theme_classic() +
    labs( y = "Length Class (cm)", fill = "Age",x = "Age") +
    #facet_wrap(~Year) +
    scale_x_continuous(limits = c(-1,max(plot_dat$Age)+1),
                     breaks = c(seq(0,max(plot_dat$Age)+2,by =1))
                     )+
    scale_y_continuous(
      limits = c(0, 60),  # Specify your desired limits
      breaks = seq(0, 60, 10)
    )+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_wrap(EastWest~.,scale="free_y",nrow=1)+
    ggtitle(plot_dat$ScientificName)


  ggsave(plot =grid.arrange(nrow=2,p2,p1),paste("figures/Cohortplots/Cohort_plot_",i,".png",sep=""),width = 14, height = 12, dpi = 300)
  rm(p1,p2)
}












