################################################################################
### Load packages and utilities
################################################################################

### general package
library("tidyverse")
library("lubridate")
library("here")
library("glue")
library("geosphere")
library("pbapply")

###-----------------------------------------------------------------------------
### do not use S2 if maps package is used
library("sf")
sf::sf_use_s2(FALSE)



### project related package
library('icesDatras')

### usefull function
source("R/fun/utilities.R")

### make path
source("R/fun/make_path.R")

### source alk function
source("R/fun/compute_alk.R")

### saving figures
device_figure <- "png"
