#remotes::install_github("DTUAqua/DATRAS/DATRAS")
#remotes::install_github("casperwberg/surveyIndex/surveyIndex")
library(DATRAS)
library(surveyIndex)
library(data.table)
# library(maptools)
library(tidyverse)
library(icesDatras)

# set working directory
setwd("C:/Users/CA04/Documents/Projects/WGBEAM/AGE")

# a list of all surveys that can be downloaded from DATRAS
#options(timeout = 120)
icesDatras::getSurveyList()

# download BTS Sole
# define the year(s) of data that you would like to extract
yrs <- seq(2015, 2024)
# define the quarter of data that you would like to extract
qrs <- seq(1:4)

# get all data from year 2012 to 2022 (BTS)
#allBTSdat <- getDatrasExchange("BTS", years = yrs, quarters = qrs, strict = FALSE)

#save(allBTSdat,  file = "BTS_Download2015-2024.RData")
# Source the function file
#source("~/Projects/WGBEAM/AGE/DatrasFunctions.R")

# Now you can call the function from DatrasFunction.R
#dd_BTS <- HL_DATRAS_DOWNLOAD(yrs, "BTS", output_file = "Sole_BTS_international_2012-2024.csv")
load(file = "BTS_Download2015-2024.RData")
# define the speceies that you would like to extract
species = "Scophthalmus maximus"
#c("Solea solea", "Microstomus kitt", "Pleuronectes platessa", "Limanda limanda", "Scophthalmus maximus", "Scophthalmus rhombus")
# define the country that you would like to extract
country = c("NL", "DE", "UK", "BE")
# subset certain species -> sole
d.spec <- sapply(species, function(s) subset(allBTSdat, Species == s))
#SPEC <- "Solea solea"

# CVs calculation based on the sole sampling scheme:

# load libraries
library(DATRAS)
library(tidyr)
library(data.table)
library(mapdata)
library(tidyverse)

#SPEC <- "Solea solea"
#load(paste0(getwd(), "/data/Sole_BTS_international_2012-2024.RData"))
# calculating CV - West
calculate_cv <- function(dat, year) {
  
  # Filter the dataset for the given year
  dat_year <- dat %>% filter(Year == year)
  
  # Create age table with length class and age
  dat_age <- dat_year %>%
    group_by(LngtCm, Age) %>%
    mutate(l = n())
  
  agetable <- reshape2::dcast(dat_age, LngtCm ~ Age)
  num_cols <- ncol(agetable) # the number of columns
  
  agetable$A <- apply(agetable[, 2:(num_cols-1)], 1, FUN = sum)  # Fish with age data
  agetable$L <- apply(agetable[, 2:(num_cols)], 1, FUN = sum)  # Fish with length data
  
  # Total number of fish with age and length data
  AL <- sum(agetable$A, na.rm = TRUE)
  LL <- sum(agetable$L, na.rm = TRUE)
  
  # Theta calculation function
  thetafn <- function(age){
    AL <- sum(agetable$A) # number of fish with age data 
    LL <- sum(agetable$L) # number of fish with length data
    
    pcol <- which(paste0(age) == colnames(agetable))
    (theta = sum((agetable[, pcol]/agetable$A)*(agetable$L/LL), na.rm = TRUE))
    return(theta)
  }
  
  # Calculate theta for each age
  thetalist <- c()
  for(i in 0:10){
    thetalist[i+1] <- thetafn(i)
  }
  
  # SE calculation function
  SE <- function(age){
    AL <- sum(agetable$A) # number of fish with age data 
    LL <- sum(agetable$L) # number of fish with length data
    
    alpal <- agetable$L/LL
    pcol <- which(paste0(age) == colnames(agetable))
    thetala <- agetable[, pcol]/agetable$A
    
    Va <- round(sum((alpal*thetala*(1-thetala)), na.rm = TRUE), 4)
    Ba <- round(sum(alpal*(thetala-thetalist[age+1])^2, na.rm = TRUE), 4)
    
    se <- sqrt(Va/AL + Ba/LL)
    
    return(se)
  }
  
  # Calculate SE for each age
  SElist <- c()
  for(i in 0:10){
    SElist[i+1] <- SE(i)
  }
  
  # Calculate CV for each age
  cv <- sqrt(SElist^2 / thetalist^2)
  
  # Create a dataframe with the results for the current year
  data <- data.frame(
    year = year,
    age = 0:10,
    SE = SElist,
    theta = thetalist,
    cv = cv
  )
  
  return(data)
}

for (SPEC in species) {
  print(SPEC)
  dd_BTS_spp = d.spec[[SPEC]]

  # Data editing
 # check length data in CA file
dd_BTS_spp[[1]]$LngtCm <- floor(dd_BTS_spp[[1]]$LngtCm)
dd_BTS_spp[[3]]$LngtCm <- floor(dd_BTS_spp[[3]]$LngtCm)

# merge hh and hl data
dd_hh_hl <- merge(dd_BTS_spp[[3]][, c("haul.id", "Year", "LngtCm", "Count")],
                  dd_BTS_spp[[2]][, c("haul.id", "ShootLat", "ShootLong", "Year", "HaulDur", "StatRec", "Depth", "HaulVal")],
                  by = c("haul.id", "Year"),
                  all.x = TRUE)
length(unique(dd_hh_hl$haul.id)) # 5494

dd_hh_hl <- dd_hh_hl %>%
  filter(!is.na(Count))
summary(dd_hh_hl)

# merge hh and ca data
summary(dd_BTS_spp[[2]])
summary(dd_BTS_spp[[1]])

dd_hh_ca <- merge(dd_BTS_spp[[2]][, c("haul.id", "ShootLat", "ShootLong", "Year", "HaulDur", "StatRec", "Depth", "HaulVal")],
                  dd_BTS_spp[[1]][, c("haul.id", "Year", "StatRec", "Age", "IndWgt", "LngtCm")],
                  by = c("haul.id", "Year"), # original code had StatRec which resulted in no information for UK
                  all.y = TRUE)
summary(dd_hh_ca)

# Extend the hh_hl data according to the number of count
dd_hh_hl$Count <- as.integer(round(dd_hh_hl$Count))
ex_hh_hl <- uncount(dd_hh_hl, weights = Count)
summary(ex_hh_hl)

length(unique(dd_hh_ca$haul.id)) # 5188
length(unique(ex_hh_hl$haul.id)) # 5415

ex_hh_hl <- ex_hh_hl %>%
  group_by(haul.id, LngtCm) %>%
  mutate(row_n = row_number()) # because of the extension, there will be duplicated rows for other data format

dd_hh_ca <- dd_hh_ca %>%
  group_by(haul.id, LngtCm) %>%
  mutate(row_n = row_number()) 

# join the data and create the final dataset before analysis
dat <- full_join(ex_hh_hl, dd_hh_ca, by = c("haul.id", "Year", "ShootLat", "ShootLong", "HaulDur",
                                            "Depth", "HaulVal", "LngtCm", "row_n")) #"StatRec",

# separate west and east data and only select area inside 51N-57.5N
dat_w1 <- dat %>%
  filter(ShootLong > -2 & ShootLong < 3 & ShootLat >= 51 & ShootLat <= 57.5)

dat_w2 <- dat %>%
  filter(ShootLong > -5 & ShootLong < 3 & ShootLat >= 55.5)

dat_w <- full_join(dat_w1, dat_w2)
rm(dat_w1, dat_w2)

ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = dat_w, aes(x = ShootLong, y = ShootLat), color = "red", size = 2) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 59)) +
  labs(title = "Western North Sea Region",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

dat_e <- dat %>%
  filter(ShootLong >= 3 & ShootLat >= 51 & ShootLat <= 57.5)

ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = dat_e, aes(x = ShootLong, y = ShootLat), color = "red", size = 2) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 59)) +
  labs(title = "Eastern North Sea Region",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

dat_C1 <- dat %>%
  filter(ShootLong < -2 & ShootLat <= 55.5)

dat_C2 <- dat %>%
  filter(ShootLat <= 51)

dat_Celtic <- full_join(dat_C1, dat_C2)
rm(dat_C1, dat_C2)

ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = dat_Celtic, aes(x = ShootLong, y = ShootLat), color = "red", size = 2) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 59)) +
  labs(title = "Celtic Sea & Irish Sea Region",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

lenRan <- c(min(dat$LngtCm, na.rm = T), max(dat$LngtCm, na.rm = T))
ageRan <- c(min(dat$Age, na.rm = T), max(dat$Age, na.rm = T))

#'@WEST 
#### ALK GRAPHS ####
# ALK table -- West
tryCatch({
  # Attempt to reshape the data using dcast
  agetable.w <- reshape2::dcast(dat_w, LngtCm ~ Age, value.var = "IndWgt", fun.aggregate = sum)
  #agetable.w <- reshape2::dcast(dat_w, LngtCm~Age)
  dim(agetable.w)
  agetable.w$A <- apply(agetable.w[, c(2:22)], 1, FUN = sum)
  agetable.w$L <- apply(agetable.w[, c(2:23)], 1, FUN = sum)
  
}, error = function(e) {
  # If there is an error, return a message or a default value
  message("An error occurred during dcast: ", e$message)
  return(NULL)  # You can return a default value like NA or an empty dataframe
}, warning = function(w) {
  # Optionally handle warnings here
  message("A warning occurred: ", w$message)
  return(NULL)  # Or return a default value
})
 
# visualize the data - West
# Compare the plaice in the east and in the west (different growth rate)
aged.dat.w <- dat_w %>%
  filter(!is.na(Age))

ggplot(aged.dat.w, aes(LngtCm, fill = factor(Age), color = factor(Age))) +
  geom_histogram(alpha = 0.5,  binwidth = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(dat$LngtCm, na.rm = T), max(dat$LngtCm, na.rm = T), by = 2), limits = c(lenRan)) +
  labs(x = "Fish length (cm)", fill = "Age", color = "Age", title = paste0(SPEC,"\n", "BTS Western North Sea"))

ggsave(filename=paste0(SPEC, "BTS_Western_NorthSea_BarPlot.png"),width = 8, height = 6, dpi = 300)


aged.dat.w$Age <- as.factor(aged.dat.w$Age)
aged.dat.w$Year <- as.factor(aged.dat.w$Year)
ggplot(aged.dat.w, aes(x = Age, y = LngtCm, fill = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = paste0(SPEC,"\n", "BTS Western North Sea"), y = "Length Class (cm)") +
  #facet_wrap(~Year) +
  scale_x_discrete(limits = levels(factor(dat$Age)))+
  scale_y_continuous(
    limits = c(0, 60),  # Specify your desired limits
    breaks = seq(0, 60, 10) 
  )

ggsave(paste0(SPEC, "BTS_Western_NorthSea_BoxPlot.png"), width = 8, height = 6, dpi = 300)

years <- 2020:2024

results_list_W <- lapply(years, function(y) calculate_cv(dat_w, y))
# Combine all the results into a single dataframe
WESTcombined_data <- do.call(rbind, results_list_W)

df_wide <- WESTcombined_data %>%
  select(year, age, cv) %>%
  pivot_wider(names_from = year, values_from = cv)

write.csv(df_wide, paste0(SPEC,"results_BTS_CV_international data_west_2020-2024.csv"))

WESTcombined_data_clean <- WESTcombined_data |> dplyr::filter(!is.nan(cv))

ggplot(WESTcombined_data_clean, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = paste0(SPEC,  " CV by Age","\n", "BTS Western North Sea (DE BE UK NL)"),
    x = "Age",
    y = "Coefficient of Variation (mCV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  #scale_y_continuous(breaks = seq(0, 2, by = 0.25), limits = c(0, 2)) +
  theme_classic()

ggsave(paste0(SPEC, "BTS_Western_NorthSea_CVbyAge.png"),   width = 8, height = 6, dpi = 300)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'@EAST
# ALK table -- East
tryCatch({
  # Attempt to reshape the data using dcast
  agetable.e <- reshape2::dcast(dat_e, LngtCm~Age)
  agetable.e$A <- apply(agetable.e[, c(2:19)], 1, FUN = sum)
  agetable.e$L <- apply(agetable.e[, c(2:20)], 1, FUN = sum)
  
}, error = function(e) {
  # If there is an error, return a message or a default value
  message("An error occurred during dcast: ", e$message)
  return(NULL)  # You can return a default value like NA or an empty dataframe
}, warning = function(w) {
  # Optionally handle warnings here
  message("A warning occurred: ", w$message)
  return(NULL)  # Or return a default value
})

# visualize the data - East
# Compare the plaice in the east and in the west (different growth rate)
aged.dat.e <- dat_e %>%
  filter(!is.na(Age))

ggplot(aged.dat.e, aes(LngtCm, fill = factor(Age), color = factor(Age))) +
  geom_histogram(alpha = 0.5,  binwidth = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(dat$LngtCm, na.rm = T), max(dat$LngtCm, na.rm = T), by = 2), limits = c(lenRan)) +
  labs(x = "Fish length (cm)", fill = "Age", color = "Age", title = paste0(SPEC,"\n", "BTS Eastern North Sea") )
ggsave(paste0(SPEC, "BTS_EasternNorthSea_BarPlot.png"),   width = 8, height = 6, dpi = 300)


aged.dat.e$Age <- as.factor(aged.dat.e$Age)
aged.dat.e$Year <- as.factor(aged.dat.e$Year)
ggplot(aged.dat.e, aes(x = Age, y = LngtCm, fill = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = paste0(SPEC,"\n", "BTS Eastern North Sea"), y = "Length Class (cm)") +
  #facet_wrap(~Year) +
  scale_x_discrete(limits = levels(factor(dat$Age)))+
  scale_y_continuous(
    limits = c(0, 60),  # Specify your desired limits
    breaks = seq(0, 60, 10) 
  )
ggsave(paste0(SPEC, "BTS_EasternNorthSea_BoxPlot.png"),   width = 8, height = 6, dpi = 300)

# calculating CV - East
years <- 2020:2024
# Loop over the years and store the results in a list
results_list_E <- lapply(years, function(y) calculate_cv(dat_e, y))
# Combine all the results into a single dataframe
EASTcombined_data <- do.call(rbind, results_list_E)

df_wide <- EASTcombined_data %>%
  select(year, age, cv) %>%
  pivot_wider(names_from = year, values_from = cv)

write.csv(df_wide, paste0(SPEC,"results_BTS_international_east_2020-2024.csv"))

EASTcombined_data_clean <- EASTcombined_data |> dplyr::filter(!is.nan(cv))
ggplot(EASTcombined_data_clean, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = paste0(SPEC,"CV by Age","\n", "BTS Eastern North Sea (DE BE UK NL)"),
    x = "Age",
    y = "Coefficient of Variation (mCV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  #scale_y_continuous(breaks = seq(0, 2, by = 0.25), limits = c(0, 2)) +
  theme_classic()
ggsave(paste0(SPEC, "BTS_EasternNorthSea_CVbyAge.png"),   width = 8, height = 6, dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'@CELTIC
# ALK table -- Celtic
tryCatch({
  # Attempt to reshape the data using dcast
  agetable.c <- reshape2::dcast(dat_Celtic, LngtCm ~ Age, value.var = "IndWgt", fun.aggregate = sum)
  #agetable.w <- reshape2::dcast(dat_Celtic, LngtCm~Age)
  dim(agetable.c)
  agetable.c$A <- apply(agetable.c[, c(2:22)], 1, FUN = sum)
  agetable.c$L <- apply(agetable.c[, c(2:23)], 1, FUN = sum)
  
  
}, error = function(e) {
  # If there is an error, return a message or a default value
  message("An error occurred during dcast: ", e$message)
  return(NULL)  # You can return a default value like NA or an empty dataframe
}, warning = function(w) {
  # Optionally handle warnings here
  message("A warning occurred: ", w$message)
  return(NULL)  # Or return a default value
})

# visualize the data - West
# Compare the plaice in the east and in the west (different growth rate)
aged.dat.C <- dat_Celtic %>%
  filter(!is.na(Age))

ggplot(aged.dat.C, aes(LngtCm, fill = factor(Age), color = factor(Age))) +
  geom_histogram(alpha = 0.5,  binwidth = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(dat$LngtCm, na.rm = T), max(dat$LngtCm, na.rm = T), by = 2), limits = c(lenRan)) +
  labs(x = "Fish length (cm)", fill = "Age", color = "Age", title = paste0(SPEC, "\n","BTS Celtic Sea & Irish Sea") )
ggsave(paste0(SPEC, "BTS_Celtic_BarPlot.png"),   width = 8, height = 6, dpi = 300)


aged.dat.C$Age <- as.factor(aged.dat.C$Age)
aged.dat.C$Year <- as.factor(aged.dat.C$Year)
ggplot(aged.dat.C, aes(x = Age, y = LngtCm, fill = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = paste0(SPEC,"\n", "BTS Celtic Sea & Irish Sea"), y = "Length Class (cm)") +
  #facet_wrap(~Year) +
  scale_x_discrete(limits = levels(factor(dat$Age)))+
  scale_y_continuous(
    limits = c(0, 60),  # Specify your desired limits
    breaks = seq(0, 60, 10) 
  )

ggsave(paste0(SPEC, "BTS_CelticSea_BoxPlot.png"),   width = 8, height = 6, dpi = 300)

years <- 2020:2024
# Loop over the years and store the results in a list
results_list_C <- lapply(years, function(y) calculate_cv(dat_Celtic, y))
# Combine all the results into a single dataframe
CELTICcombined_data <- do.call(rbind, results_list_C)

df_wide <- CELTICcombined_data %>%
  select(year, age, cv) %>%
  pivot_wider(names_from = year, values_from = cv)

write.csv(df_wide, paste0(SPEC,"results_BTS_CV_international data_Celtic_2020-2024.csv"))

CELTICcombined_data_clean <- CELTICcombined_data |> dplyr::filter(!is.nan(cv))
ggplot(CELTICcombined_data_clean, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = paste0(SPEC," CV by Age", "\n", "BTS Celtic Sea & Irish Sea (DE BE UK NL)"),
    x = "Age",
    y = "Coefficient of Variation (mCV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  #scale_y_continuous(breaks = seq(0, 2, by = 0.25), limits = c(0, 2)) +
  theme_classic()

ggsave(paste0(SPEC, "BTS_CelticSea_CVbyAge.png"),   width = 8, height = 6, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'@Combined
# ALK table -- Combined
tryCatch({
  # Attempt to reshape the data using dcast
  agetable <- reshape2::dcast(dat, LngtCm ~ Age, value.var = "IndWgt", fun.aggregate = sum)
  dim(agetable)
  agetable$A <- apply(agetable[, c(2:22)], 1, FUN = sum)
  agetable$L <- apply(agetable[, c(2:23)], 1, FUN = sum)
}, error = function(e) {
  # If there is an error, return a message or a default value
  message("An error occurred during dcast: ", e$message)
  return(NULL)  # You can return a default value like NA or an empty dataframe
}, warning = function(w) {
  # Optionally handle warnings here
  message("A warning occurred: ", w$message)
  return(NULL)  # Or return a default value
})

# visualize the data - West
# Compare the plaice in the east and in the west (different growth rate)
aged.dat <- dat %>%
  filter(!is.na(Age))

ggplot(aged.dat, aes(LngtCm, fill = factor(Age), color = factor(Age))) +
  geom_histogram(alpha = 0.5,  binwidth = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(dat$LngtCm, na.rm = T), max(dat$LngtCm, na.rm = T), by = 2), limits = c(lenRan)) +
  labs(x = "Fish length (cm)", fill = "Age", color = "Age", title = paste0(SPEC, "\n","BTS") )
ggsave(paste0(SPEC, "BTS_BarPlot.png"),   width = 8, height = 6, dpi = 300)


aged.dat$Age <- as.factor(aged.dat$Age)
aged.dat$Year <- as.factor(aged.dat$Year)
ggplot(aged.dat, aes(x = Age, y = LngtCm, fill = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = paste0(SPEC,"\n", "BTS"), y = "Length Class (cm)") +
  #facet_wrap(~Year) +
  scale_x_discrete(limits = levels(factor(dat$Age)))+
  scale_y_continuous(
    limits = c(0, 60),  # Specify your desired limits
    breaks = seq(0, 60, 10) 
  )

ggsave(paste0(SPEC, "BTS_BoxPlot.png"),   width = 8, height = 6, dpi = 300)

years <- 2020:2024
# Loop over the years and store the results in a list
results_list <- lapply(years, function(y) calculate_cv(dat, y))
# Combine all the results into a single dataframe
combined_data <- do.call(rbind, results_list)

df_wide <- combined_data %>%
  select(year, age, cv) %>%
  pivot_wider(names_from = year, values_from = cv)

write.csv(df_wide, paste0(SPEC,"results_BTS_CV_international data_2020-2024.csv"))

combined_data_clean <- combined_data |> dplyr::filter(!is.nan(cv))
ggplot(combined_data_clean, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = paste0(SPEC," CV by Age","\n", "BTS (DE BE UK NL)"),
    x = "Age",
    y = "Coefficient of Variation (mCV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  #scale_y_continuous(breaks = seq(0, 2, by = 0.25), limits = c(0, 2)) +
  theme_classic()

ggsave(paste0(SPEC, "BTS_CVbyAge.png"),   width = 8, height = 6, dpi = 300)


}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# ONLY THE DUTCH DATA
# load data
library(readxl)
nl.bts.sole <- dat_e #read.csv(paste0(SPEC,"BTS_CV_east_2020-2024.csv"))

colnames(nl.bts.sole) <- gsub("^X", "", colnames(nl.bts.sole))
nl.bts.sole <- nl.bts.sole[, -1] 
# transform to a long dataset
nl.bts.sole_long <- nl.bts.sole %>%
  pivot_longer(cols = -Age,         # All columns except "age" will be pivoted
               names_to = "Year",   # New column for year
               values_to = "cv")

ggplot(nl.bts.sole_long, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = "BTS East (NL) Sole CV by Age",
    x = "Age",
    y = "Coefficient of Variation (CV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  theme_minimal()

# BTS NL WEST
nl.bts.sole.w <- read.csv("C:/Users/chin008/OneDrive - Wageningen University & Research/git/otolith_optimization/results/BTS_CV_Sole_west_2020-2024.csv")

colnames(nl.bts.sole.w) <- gsub("^X", "", colnames(nl.bts.sole.w))
nl.bts.sole.w <- nl.bts.sole.w[, -1] 
# transform to a long dataset
nl.bts.sole.w_long <- nl.bts.sole.w %>%
  pivot_longer(cols = -age,         # All columns except "age" will be pivoted
               names_to = "year",   # New column for year
               values_to = "cv")

ggplot(nl.bts.sole.w_long, aes(x = age, y = cv, color = factor(year))) +
  geom_line() + # Line plot for CV distribution by age
  geom_point() + # Optional: add points for better visibility
  labs(
    title = "BTS West (NL) Sole CV by Age",
    x = "Age",
    y = "Coefficient of Variation (CV)",
    color = "Year"
  ) +
  geom_hline(yintercept = 0.25, color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 1.6, by = 0.25), limits = c(0, 1.6)) +
  theme_minimal()
