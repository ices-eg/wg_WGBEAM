################################################################################
### Load ices rectangle and areas
### downloaded from https://gis.ices.dk/shapefiles/ICES_rectangles.zip
### https://gis.ices.dk/shapefiles/ICES_areas.zip
################################################################################

###-----------------------------------------------------------------------------
### load ices rectangle
sf_ices_rec <- st_read(here("data", "raw", "ices_rectangles/ICES_Statistical_Rectangles_Eco.shp"))  %>%
  st_transform(x = . , crs = 4326)

###-----------------------------------------------------------------------------
### load areas
sf_ices_areas <- st_read(here("data", "raw", "ices_areas/ICES_Areas_20160601_cut_dense_3857.shp"))
sf_ices_areas <- st_transform(x = sf_ices_areas , crs = 4326)
