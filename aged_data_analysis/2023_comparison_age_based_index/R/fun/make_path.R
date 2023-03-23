################################################################################
### Set path for data
################################################################################

### create directory if not already created
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/tidy", showWarnings = FALSE)

###-----------------------------------------------------------------------------
### personal database
### checks if data_base folder exists and set it as data_raw if TRUE
path_data_raw <- "C:/Users/jblecomt/data_base"
if (dir.exists(path_data_raw)) {
  path_data_raw <- "C:/Users/jblecomt/data_base"
} else {
  path_data_raw <- here("data", "raw")
  dir.create(path_data_raw, showWarnings = FALSE)
  ### create directories to store raw data if not already there
  dir.create(here(path_data_raw, "ices_areas"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "ices_rectangles"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "vms_3_3"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "sacrois_flux"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "ref_species"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "ref_navs"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "obsvente"), showWarnings = FALSE)
  dir.create(here(path_data_raw, "obsmer"), showWarnings = FALSE)
}

###-----------------------------------------------------------------------------
path_data_wh <- "C:/Users/jblecomt/WGBIE/sol_8ab/sole_27_8ab_data_wharehouse"

if (dir.exists(path_data_wh)) {
  path_data_tidy <- path_data_wh
} else {
  path_data_tidy <- here("data", "tidy")
}

###-----------------------------------------------------------------------------
###
path_tidy_plots <- here("data", "tidy", "plots")
dir.create(path_tidy_plots, showWarnings = FALSE)

###-----------------------------------------------------------------------------
###
path_figures <- here("figures")
dir.create(path_figures, showWarnings = FALSE)
