################################################################################
### make a map of hauls for the selected year
################################################################################

library("sf")
sf::sf_use_s2(FALSE)
library("maps")
library('mapdata')

###-----------------------------------------------------------------------------
### converting as sf object
sf_hh <- st_as_sf(df_hh, coords = c("HaulLong","HaulLat"),
           crs = "+proj=longlat +datum=WGS84")


###-----------------------------------------------------------------------------
### coast line
limits_lon <- c(-5, 0)
limits_lat <- c(45, 49)
coast_maps <- maps::map("worldHires",
                      xlim = limits_lon,
                      ylim = limits_lat,
                      col = "gray90",
                      fill = TRUE)

sf_coast_maps <- sf::st_as_sf(coast_maps,
                              crs = "+proj=longlat +datum=WGS84")



###-----------------------------------------------------------------------------
### make plot
plot_map_hh <- ggplot() +
  geom_sf(data = sf_hh, aes(colour = HaulVal), shape = '+', size = 5) +
  geom_sf(data = sf_coast_maps, fill = 'grey') +
  coord_sf(crs = st_crs(sf_coast_maps), expand = FALSE,
           xlim = limits_lon, ylim = limits_lat) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        plot.background = element_rect(fill = NA, colour = 'transparent'),
        legend.background = element_blank(),
        legend.key = element_blank()) + xlab('Longitude') +
  ylab('Latitude')
print(plot_map_hh)
