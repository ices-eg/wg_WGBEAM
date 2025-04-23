library(icesDatras)
library(tidyverse)
library(sp)
library(sf)
## Fortify makes a really shitty version of the DF, this function retains all the important information from a shapefile #'*Zachary Radford code*
makeReadableFortify <- function(shapefile) {
  shapefile@data$id <- rownames(shapefile@data)
  shapefile.points <- fortify(shapefile)
  shapefile.df <- left_join(shapefile.points, shapefile@data, by = "id")
  return(shapefile.df) #'
}

## ICES Area 
ICESArea <- spTransform(as_Spatial(read_sf(dsn = paste("~/Projects/WGBEAM/Shapefile", sep=""),layer='ICES_Areas_20160601_cut_dense_3857')), CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ICESArea <- makeReadableFortify(ICESArea)

AreaICES <- ICESArea |> dplyr::select(Area_27) |> distinct()


europe <- spTransform(as_Spatial(read_sf(dsn = paste("~/Projects/WGBEAM/Shapefile", sep=""),layer='europe')), CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#shapefile('Shapefile/europe.shp')
europe <- makeReadableFortify(europe)

survey = c("NS-IBTS")
year = c(2015:2025)

species <- c("Sepia", "Sepia elegans", "Sepia officinalis", 
             "Loligo",  "Loligo forbesii", "Loligo vulgaris",
             "Illex", "Illex coindetii", "Illex illecebrosus") 
ID <- c(138477, 141443, 141444,
        138139, 140270, 140271,
        138278, 140621, 153087) 
        
sppID <- data.frame(species = species,  Valid_Aphia = ID)

hh <- NA
hl<- NA
# Can only work with singlar and isnt working anyway. returns NA. CPUE <- icesDatras::getCPUELength(survey = survey,year = year, quarter = 1:4)# Error in sprintf("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getCPUELength?survey=%s&year=%i&quarter=%i",  : arguments cannot be recycled to the same length
for (s in survey){
HH <- icesDatras::getDATRAS(record = "HH", survey = s,years = year, quarters = 1:4) 
HL <- icesDatras::getDATRAS(record = "HL", survey = s,years = year, quarters = 1:4) 
hh<- rbind(HH,hh)
hl<- rbind(HL,hl)
}


HH <- hh 

HH <- HH |> dplyr::select(-RecordType, -DateofCalculation)
HH <- HH|> dplyr::mutate(HaulID = as.character(paste(HH$Survey, HH$Year, HH$Quarter, HH$Country, HH$Ship, HH$Gear, HH$StNo, HH$HaulNo, sep = ":")))
                                                                                                                                                     
HL <- hl 

HL <- HL |> dplyr::select(-RecordType, -DateofCalculation) 
HL <- HL |> dplyr::mutate(HaulID = as.character(paste(HL$Survey, HL$Year, HL$Quarter, HL$Country, HL$Ship, HL$Gear, HL$StNo, HL$HaulNo, sep = ":")) )
                                                                                                                                  
TOTHL <- HL |> dplyr::select(Survey, Quarter, Country, Ship, Gear, SweepLngt, GearEx, DoorType, StNo, HaulNo, Year , TotalNo, CatCatchWgt, Valid_Aphia, HaulID) |> distinct()

TOTHL <- TOTHL |> dplyr::filter(Valid_Aphia %in% ID)
  
TOTHL <-left_join(TOTHL, sppID)
  
HLHH <- full_join(HH, TOTHL)

HLHH[HLHH == -9] <- NA

HLHH <- HLHH |> dplyr::mutate(kmSqu = (Distance/1000) * DoorSpread) 

HLHH <- HLHH |> dplyr::mutate(CPUE = TotalNo/(HaulDur/60) )

CPUE <- HLHH |> group_by(Survey, Quarter, Country, Year, Valid_Aphia, species) |> summarise(CPUE = sum(CPUE, na.rm = T)) |> na.omit()

library(scales)
unique(CPUE$Country)

cbPalReg <- c("#009E73", "#9C2887", "#F56300","#D8334A","#56B4E9",  "#CC79A7" , "#F0E442", "#0072B2" )
Colourfiles <- data.frame(cbPalReg, c("DE", "DK", "FR", "GB-SCT", "NL", "NO", "SE", "GB"))
colnames(Colourfiles) <- c("colour", "Country")
CPUEa <- CPUE

CPUE <- CPUE |> dplyr::filter(Survey == "NS-IBTS")

for (s in species)   {
  
  CPUE_Spp <- CPUE |> dplyr::filter(ScientificName == s) |> group_by(Year, Quarter, Valid_Aphia, ScientificName) |> summarise(CPUE = sum(CPUE, na.rm = T)) 
  
  ggplot(CPUE_Spp, aes(x = Year, y = CPUE)) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = paste0(unique(CPUE_Spp$ScientificName),"\n","Total annual catch" ), 
         x = "Year", y = "CPUE (no/hr)") + # (no/km²
    labs(caption = unique(CPUE_Spp$Valid_Aphia))+
    facet_wrap(~Quarter)+
    scale_y_continuous(labels = comma) +  # Format y-axis labels
    scale_x_continuous(breaks = seq(2015, 2024, by = 1), limits = c(2015, 2024)) +
    theme_bw()
  ggsave(filename= paste0("TimeSeries/TimeSeries_", s,".png"), width = 8, height = 5)
  #print(p
  CPUE_Spp2 <- CPUE |> dplyr::filter(ScientificName == s) |> group_by(Year, Quarter, Country, Valid_Aphia, ScientificName) |> summarise(CPUE = sum(CPUE, na.rm = T)) 
  
  ggplot() +
    geom_col(CPUE_Spp2, mapping = aes(x = Year, y = CPUE, fill = Country)) +
    labs(title = paste0(unique(CPUE_Spp2$ScientificName),"\n","Total annual catch" ), 
         x = "Year", y = "CPUE (no/hr)") + # (no/km²
    labs(caption = unique(CPUE_Spp2$Valid_Aphia))+
    facet_wrap(~Quarter)+
    scale_y_continuous(labels = comma) +  # Format y-axis labels
    scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
    scale_fill_manual(values=setNames(Colourfiles$colour,Colourfiles$Country)) +
    theme_bw()
  ggsave(filename= paste0("TimeSeries/Country_", s,"_TimeSeries.png"), width = 8, height = 5)
  
}
 
CPUE$GroupSpp <- ifelse(CPUE$ScientificName == "Sepia","All Sepia spp", ifelse(CPUE$ScientificName == "Sepia elegans","All Sepia spp", ifelse(CPUE$ScientificName == "Sepia officinalis","All Sepia spp", ifelse(CPUE$ScientificName ==  "Loligo","All Loligo spp", ifelse(CPUE$ScientificName == "Loligo forbesii","All Loligo spp", ifelse(CPUE$ScientificName == "Loligo vulgaris","All Loligo spp", ifelse(CPUE$ScientificName == "Illex","All Illex spp", ifelse(CPUE$ScientificName ==  "Illex coindetii","All Illex spp", ifelse(CPUE$ScientificName ==  "Illex illecebrosus" ,"All Illex spp","Other")))))))))

CPUE <-  CPUE |> dplyr::filter(Quarter != 2) |> dplyr::filter(!is.na(GroupSpp)) |> dplyr::filter((GroupSpp) != "Other")
       
                    
for (gg in unique(CPUE$GroupSpp))   {

  CPUE_Spp <- CPUE |> dplyr::filter(GroupSpp == gg) |> group_by(Year, Quarter, ScientificName, Valid_Aphia, GroupSpp) |> summarise(CPUE = sum(CPUE, na.rm = T)) 
  
  ggplot(CPUE_Spp, aes(x = Year, y = CPUE)) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = paste0(unique(CPUE_Spp$GroupSpp),"\n","Total annual catch" ), 
         x = "Year", y = "CPUE (no/hr)") + # (no/km²
    facet_wrap(~Quarter)+
     scale_y_continuous(labels = comma) +  # Format y-axis labels
    scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
    theme_bw()
  ggsave(filename= paste0( "TimeSeries/TimeSeries_", gg,".png"), width = 8, height = 5)
  
  CPUE_Spp2 <- CPUE |> dplyr::filter(GroupSpp == gg) |> dplyr::group_by(Year, Quarter, ScientificName,Valid_Aphia, GroupSpp) |> dplyr::summarise(CPUE = sum(CPUE, na.rm = T)) 
  
  ggplot() +
    geom_col(CPUE_Spp2, mapping = aes(x = Year, y = CPUE, fill = ScientificName)) +
    labs(title = paste0(unique(CPUE_Spp2$GroupSpp),"\n","Total annual catch" ), 
         x = "Year", y = "CPUE (no/hr)") + # (no/km²
    scale_y_continuous(labels = comma) +  # Format y-axis labels
    scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
    facet_wrap(~Quarter)+
   scale_fill_manual(values=c("#009E73", "#F56300", "#56B4E9"))+#setNames(Colourfiles$colour,Colourfiles$Country)) +
    theme_bw()
  ggsave(filename= paste0("TimeSeries/Species_", gg,"_TimeSeries.png"), width = 10, height = 5)
}




LocCPUE <- HLHH |> group_by(Survey, Quarter, Year, Valid_Aphia, ScientificName, ShootLat, ShootLong) |> summarise(CPUE = sum(CPUE, na.rm = T)) |> na.omit()



LocCPUE$GroupSpp <- ifelse(LocCPUE$ScientificName == "Sepia","All Sepia spp", ifelse(LocCPUE$ScientificName == "Sepia elegans","All Sepia spp", ifelse(LocCPUE$ScientificName == "Sepia officinalis","All Sepia spp", ifelse(LocCPUE$ScientificName ==  "Loligo","All Loligo spp", ifelse(LocCPUE$ScientificName == "Loligo forbesii","All Loligo spp", ifelse(LocCPUE$ScientificName == "Loligo vulgaris","All Loligo spp", ifelse(LocCPUE$ScientificName == "Illex","All Illex spp", ifelse(LocCPUE$ScientificName ==  "Illex coindetii","All Illex spp", ifelse(LocCPUE$ScientificName ==  "Illex illecebrosus" ,"All Illex spp","Other")))))))))

LocCPUE <-  LocCPUE |> dplyr::filter(Quarter != 2) |> dplyr::filter(!is.na(GroupSpp)) |> dplyr::filter((GroupSpp) != "Other") |> dplyr::filter(Survey == "NS-IBTS")


for (gg in unique(LocCPUE$GroupSpp))   {
    
  LocCPUE_group <- LocCPUE |> dplyr::filter(GroupSpp == gg) |> dplyr::group_by(Year, Quarter, ScientificName,Valid_Aphia, GroupSpp, ShootLat, ShootLong) |> dplyr::summarise(CPUE = sum(CPUE, na.rm = T)) |> na.omit() 
  
  
  xlim = c(min(LocCPUE_group$ShootLong), max(LocCPUE_group$ShootLong))
  ylim =c(min(LocCPUE_group$ShootLat),max(LocCPUE_group$ShootLat))
  
  HH2024<- HH |> dplyr::filter(Year == 2024) |> dplyr::filter(Quarter != 2) |> dplyr::filter(Quarter != 4)  |> dplyr::filter(Survey == "NS-IBTS")
  CPUELoc2024<-LocCPUE_group |> dplyr::filter(Year == 2024) 
  unique(CPUELoc2024$Quarter)
  ## CPUE - Biomass 
  ggplot()+
    ggtitle(paste0(gg,"\n","Distribution (2024)"))+
    labs(subtitle =  "Raised number", colour = "Surveys", size = "Catch (Number/hr)")+
    geom_polygon(data = europe, aes(long, lat, group = group), fill = "grey")+ 
    geom_polygon(data = ICESArea, aes(long, lat, group = group), fill = NA, colour = "black", size = 0.5) +
    geom_point(data = HH2024,  aes(x = ShootLong, y = ShootLat), stat = "identity", alpha = 0.25, size = 0.25) + 
    geom_point(data = CPUELoc2024,  aes(x = ShootLong, y = ShootLat, size = CPUE, colour = ScientificName), stat = "identity", alpha = 0.5) +
    scale_colour_manual(values=c("#009E73", "#F56300", "#56B4E9"))+#scale_colour_manual(values=cbPalReg) +
    facet_wrap(~Quarter)+#
    scale_size_continuous(range = c(2, 10)) +  # Adjust size range here
    coord_quickmap(xlim = xlim, ylim = ylim) +
    theme_bw()+
    theme(plot.title = element_text(face="bold"), panel.grid.minor = element_line(colour = "NA"), )+
    xlab("Longitude") +
    ylab("Latitude") 
  
  ggsave(filename = paste("TimeSeries/Species_",gg,"2024_Distribution",".png", sep = ""), device = "png", height = 10, width= 15) 
  
  
}                                                                                                                                                    
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                                                                                                                                                                      