################################################################################
### master script to compare BTS aged data per country
################################################################################

###-----------------------------------------------------------------------------
### erase data already computed
erase_tidy_data <- TRUE

###-----------------------------------------------------------------------------
### boot r script: load package and function
source("R/fun/boot.R")

###-----------------------------------------------------------------------------
### survey code
survey_code <- "BTS"

### choose the year for which to compute the IA
year_vec <- (2015:2022)

###
quarter_vec <- c(1, 2, 3, 4)

### Species species_code from datras and scientificname from worms database
### 127143 : "Pleuronectes platessa"
### 127139 : "Limanda limanda"
species_code <- "127139"
species_name <- worms::wormsbyid(as.numeric(species_code)) %>%
  pull(scientificname)

###-----------------------------------------------------------------------------
### choose the age+ group: 4+ for the WGBEAM or 8+ for the WGBIE
max_age <- 8

###-----------------------------------------------------------------------------
### Index grouping variable
index_grouping_var <- "Country"

###-----------------------------------------------------------------------------
### Read data from datras database with icesDatras package
source("R/load_data/extract_data_from_datras.R")

###-----------------------------------------------------------------------------
### for DAB keep 4 area only
if (species_name == "Limanda limanda") {

 source("R/load_data/sf_ices_rectangle.R")

  sf_ices_area4 <- sf_ices_areas %>%
    filter(SubArea == "4")

  sf_ices_rec_area4 <- sf::st_join(sf_ices_area4,
                                   sf_ices_rec)

  df_hh <- df_hh %>%
    filter(StatRec %in% sf_ices_rec_area4$ICESNAME)

  df_hh_hl <- df_hh_hl %>%
    filter(StatRec %in% sf_ices_rec_area4$ICESNAME)

  df_hh_ca <- df_hh_ca %>%
    filter(StatRec %in% sf_ices_rec_area4$ICESNAME)
}

###-----------------------------------------------------------------------------
###
source("R/plot_data/plot_length_number.R")

###-----------------------------------------------------------------------------
### Compute mean over haul
df_alk_prop_long <- compute_alk(df_hh_ca,
                                age_plus_group = max_age,
                                index_grouping_var = "Country")

source("R/plot_data/plot_alk_prop.R")
