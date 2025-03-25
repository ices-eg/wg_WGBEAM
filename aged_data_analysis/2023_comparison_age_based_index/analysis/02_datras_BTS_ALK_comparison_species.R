################################################################################
### WGBEAM-DATRAS
### Compute ALK per species from CA tables for BTS survey for multiple species
### Author : JB Lecomte jean.baptiste.lecomte@ifremer.fr
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
year_vec <- (2020:2024)

###
quarter_vec <- c(1, 2, 3, 4)

### Species species_code from datras and scientificname from worms database
### 127143 : "Pleuronectes platessa"
### 127139 : "Limanda limanda"
###127160 : Solea solea
#species_code <- "127139"
#species_name <- worrms::wm_id2name(as.numeric(species_code)) #%>%  pull(scientificname)

# wm_records_common(name = 'sole')
# wm_records_common(name = 'lemon sole')
# wm_records_common(name = 'plaice')
# wm_records_common(name = 'dab')

stocks <- data.frame("stock.code" = c(127143, 127140, 127160,  127139),
                    "Common.name"= c("Plaice", "Lemon Sole", "Sole",  "Dab"))
path_figures <- glue("{path_figures}/{paste(stocks$Common.name, collapse = '_')}")
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

###-----------------------------------------------------------------------------
### choose the age+ group: 4+ for the WGBEAM or 8+ for the WGBIE
max_age <- 8

###-----------------------------------------------------------------------------
### Index grouping variable
index_grouping_var <- c("ScientificName", "Country")

###-----------------------------------------------------------------------------
### Read data from datras database with icesDatras package
source("R/load_data/extract_data_from_datras_all_species.R")
list_datras <-  purrr::map(list_datras, function(df) {
  if ("Valid_Aphia" %in% colnames(df)) {
    df %>% dplyr::filter(.data$Valid_Aphia %in% stocks$stock.code) %>%
      droplevels()
  } else {
    df # Return the data frame unchanged if  column is not present
  }
})
unpack_list(list_datras)
# ###-----------------------------------------------------------------------------
# ### for DAB keep 4 area only
# if (species_name == "Limanda limanda") {
#  #source("R/Loading Data/sf_ices_rectangle.R")
#
#   sf_ices_area4 <- sf_ices_areas %>%
#     filter(SubArea == "4")
#
#   sf_ices_rec_area4 <- sf::st_join(sf_ices_area4,
#                                    sf_ices_rec)
#
#   df_hh <- df_hh %>%
#     filter(StatRec %in% sf_ices_rec_area4$ICESNAME)
#
#   df_hh_hl <- df_hh_hl %>%
#     filter(StatRec %in% sf_ices_rec_area4$ICESNAME)
#
#   df_hh_ca <- df_hh_ca %>%
#     filter(StatRec %in% sf_ices_rec_area4$ICESNAME)
# }

###-----------------------------------------------------------------------------
###
source("R/plot_data/plot_length_number.R")

###-----------------------------------------------------------------------------
### Compute mean over haul
df_alk_prop_long <- compute_alk(df_hh_ca,
                                age_plus_group = max_age,
                                index_grouping_var = "Country")

source("R/plot_data/plot_alk_prop.R")

###-----------------------------------------------------------------------------
### Compute age-index as numbers/km2
source("R/process_data/compute_age_index.R")

