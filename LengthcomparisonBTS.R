# Load necessary libraries
library(tidyverse)
library(readxl)
library(scales)
library(icesDatras)
library(worrms)

setwd("C:/Users/CA04/Documents/Projects/WGBEAM")

# Function to get species name from alpha ID using worms package
get_species_name <- function(alpha_id) {
  tryCatch({
    aphia_record <- worrms::wm_id2name(id = alpha_id)
    return(aphia_record)
  }, error = function(e) {
    return(NA)
  })
}

get_Common_name <- function(alpha_id) {
  tryCatch({
    nameComm <-  worrms::wm_common_id(id = aphia_id) 
    nameComm <- nameComm[nameComm$language == "English",]
    nameComm <- nameComm$vernacular[1]
    return(nameComm)
  }, error = function(e) {
    return(NA)
  })
}

# DatrasFunction.R

# Load necessary package
library(worrms)

# Function to identify species group and separate fish by order
get_Phylum_name <- function(alpha_id) {
 
 tryCatch({
   # Query WoRMS for taxonomic information using Aphia ID
  taxon_info <- worrms::wm_records_taxamatch(alpha_id)
  
  # Check if taxon_info is empty (no record found)
  if (length(taxon_info) == 0) {
    return("Species not found in WoRMS")
  }
  
  # Extract the taxonomy information
  taxon_info <- taxon_info[[1]]
  phylum_name <- taxon_info$phylum
  class_name <- taxon_info$class
  order_name <- taxon_info$order  # Get the order of the species
  
  # Identify the group based on phylum, class, and order
  if (grepl("Chordata", phylum_name)) {
    # Identify fish
    if (grepl("Actinopterygii", class_name)| grepl("Teleostei", class_name)) { # Fish class (ray-finned)
      # Separate fish by order
      if (grepl("Pleuronectiformes", order_name)) {
        return("Flatfish (Pleuronectiformes)")
      } else if (grepl("Perciformes", order_name)) {
        return("Perciformes Fish")
      } else if (grepl("Salmoniformes", order_name)) {
        return("Salmoniformes Fish")
      } else if (grepl("Clupeiformes", order_name)) {
        return("Clupeiformes Fish")
      } else if (grepl("Cypriniformes", order_name)) {
        return("Cypriniformes Fish")
      } else if (grepl("Gadiformes", order_name)) {
        return("Gadiformes Fish")
      } else if (grepl("Lophiiformes", order_name)) {
        return("Lophiiformes Fish")
      } else {
        return(paste("Other Orders"))  # Return the order name if it is a fish but not listed
      }
    } else {
      return("Not a Ray-finned Fish")
    }
  } else if (grepl("Crustacea", phylum_name)) {
    return("Crustacean")
  } else if (grepl("Scyphozoa", phylum_name)) {
    return("Jellyfish")
  } else if (grepl("Mollusca", phylum_name) && grepl("Cephalopoda", class_name)) {
    return("Cephalopods")
  } else if (grepl("Mollusca", phylum_name)) {
    return("Mollusc (Other Molluscs)")
  } else {
    return("Other")
  }
 }, error = function(e) {
   return(NA)
 })
}

allyears <- c(1985:2024)
allsurvey <- c("BTS","SNS","DYFS")

# Initialize an empty data frame to store the final data
final_data <- data.frame()

# Loop through the years, quarters, and species
for (Y in allyears) {
  for (S in allsurvey) {
  rm(HL)
  HL <- getDATRAS(record = "HL", survey = S, years = Y, quarters= 1:4)
  
  if(length(HL)>1){
  HL$Lngtmm <- ifelse(HL$LngtCode == "1", HL$LngtClass * 10, 
                      ifelse(HL$LngtCode == "2", HL$LngtClass * 10, 
                             ifelse(HL$LngtCode == "5", HL$LngtClass * 10, 
                                    HL$LngtClass)))
  
  HL$Lngtcm <- floor(HL$Lngtmm/10)
  
  
  # Ensure the Sex column is of the same type
  HL$Sex <- as.character(HL$Sex)
  HL$StNo <- as.character(HL$StNo)
  HL$GearEx <- as.character(HL$GearEx)
  HL$LngtCode <- as.character(HL$LngtCode)
  HL$DevStage <- as.character(HL$DevStage)
  
  # Convert any -9 values to NA
  HL[HL == -9] <- NA
  
  HL <- HL %>% dplyr::filter(!is.na(HLNoAtLngt))
  # HL <- HL |> dplyr::filter(Country == "GB")  
  
  final_data <- bind_rows(final_data, HL)
  }
  }
}

final_data

# Initialize a vector of NA values
SpeciesINFO <- data.frame(Valid_Aphia = unique(final_data$Valid_Aphia), ScientificName = NA, CommonName = NA, TaxaGroup = NA)
spp_ids <- rep(NA, nrow(SpeciesINFO))
Cn_ids <- rep(NA, nrow(SpeciesINFO))
Py_ids <- rep(NA, nrow(SpeciesINFO))

# Apply the function to each species name in TOTBio$SpeciesScientificName
for (i in 1:nrow(SpeciesINFO)) {
  aphia_id <- SpeciesINFO$Valid_Aphia[i]
  spp_id <- get_species_name(aphia_id)
  spp_ids[i] <- spp_id
  
  cn_id <- get_Common_name(aphia_id)
  Cn_ids[i] <- cn_id
  
  py_id <- get_Phylum_name(spp_id)
  Py_ids[i] <- py_id
}

# Assign the result to TOTBio$Valid_Aphia
SpeciesINFO$ScientificName <- spp_ids
SpeciesINFO$CommonName <- Cn_ids
SpeciesINFO$TaxaGroup <- Py_ids

final_data <- left_join(final_data, SpeciesINFO)

write.csv(final_data, "final_dataBTS.csv", row.names = FALSE)
##########################################################################################################################
library(gridExtra)

final_data <- read.csv("final_dataBTS.csv")

final_data <- final_data %>% arrange(ScientificName)

final_data$SubFactor <- ifelse(!is.na(final_data$SubFactor),final_data$SubFactor,1)

# Initialize an empty data frame to store the results
results <- data.frame(Species = character(), CommonName = character(), TaxaGroup = character(),
                      SpeciesCode = integer(), TotNo = integer(),
                      LengthMin = numeric(), LengthMax = numeric(), TailGap =  numeric(), HeadGap = numeric(), 
                      Num_Outliers = character()
)

Outliers <- data.frame()

# Create an empty list to store ggplots
plot_list <- list()
plot_list2 <- list()
plot_list3 <- list()

#pdf("SpeciesPlots_OutlierLengths_withSubFactor.pdf") 

for (species in unique(final_data$ScientificName)) {
  rm(dataHL,HLTot,HLTot2,  min_length, max_length, complete_bins, middle_index, first_gap_after_middle, first_gap_before_middle)
  print(species)

  dataHL <- final_data %>% dplyr::filter(ScientificName == species)
  
  TaxaInfo <- unique(dataHL$TaxaGroup)
  
  HLTot2 <- dataHL %>% group_by(ScientificName, CommonName, Lngtcm) |>
    summarise(Freq_NoAtLngt = round(sum(HLNoAtLngt*SubFactor, na.rm = T),1)) |> ungroup()
  
  HLTot <- dataHL %>% group_by(ScientificName, CommonName, Lngtcm) |>
    summarise(Freq_NoAtLngt = round(sum(HLNoAtLngt, na.rm = T),1)) |> ungroup()
  
  
  # Step 1: Calculate Minimum and Maximum Length
  min_length <- min(dataHL$Lngtcm, na.rm = TRUE)
  max_length <- max(dataHL$Lngtcm, na.rm = TRUE)
  
  # Step 2: Create the complete sequence of 1cm bins
  complete_bins <- data.frame(Lngtcm = seq(min_length, max_length))
  
  # Step 3: Merge the complete bins with the summarized data
  HLTot <- left_join(complete_bins, HLTot, by = "Lngtcm")
  
  # Step 4: Fill NA values with 0 (for missing bins)
  HLTot$Freq_NoAtLngt[is.na(HLTot$Freq_NoAtLngt)] <- 0
  
  HLTot <- HLTot %>% arrange(Lngtcm)
  
  # Step 5: Identify where two consecutive bins have zero values
  HLTot <- HLTot %>%
    mutate(Zero_Freq_Pair = if_else(lag(Freq_NoAtLngt, default = 0) == 0 & Freq_NoAtLngt == 0, TRUE, FALSE))
  
  HLTot <- HLTot %>% mutate(row_num = row_number())
  
  # Step 2: Find the middle point of the dataset
  # middle_index <- nrow(HLTot) %/% 2  # This gives the middle index (integer division)
  middle_index <- sum(HLTot$Lngtcm * HLTot$Freq_NoAtLngt) / sum(HLTot$Freq_NoAtLngt)
  #  HLTot[HLTot$Freq_NoAtLngt == max(HLTot$Freq_NoAtLngt),]$Lngtcm #
  
  
  # Step 4: Find the first TRUE in Zero_Freq_Pair after the middle point
  gaps <- HLTot %>%
    dplyr::filter(Zero_Freq_Pair == TRUE) 
  
  first_gap_after_middle <- gaps  %>%
    dplyr::filter(Lngtcm > middle_index) #  after middle
  
  if (length(first_gap_after_middle) > 0 && nrow(first_gap_after_middle) > 0) {
    first_gap_after_middle <- gaps  %>%
      dplyr::filter(Lngtcm > middle_index) %>%
      slice(1) 
    
    if (!is.na(first_gap_after_middle$Lngtcm)) { 
      first_gap_after_middle <- gaps  %>%
        dplyr::filter(Lngtcm > middle_index) %>%
        slice(1) 
      MaxGap <- (first_gap_after_middle$Lngtcm)-2 #HLTot[first_gap_after_middle$Lngtcm - 2, ]$Lngtcm
    } else {
      message("No gap found after the middle point.")
      MaxGap <- NA
    }} else {
      message("No data.")
      MaxGap <- NA
    } 
  
  
  # Step 5: Find the first TRUE in Zero_Freq_Pair after the middle point
  first_gap_before_middle2 <- gaps %>%
    slice(1)
  
  first_gap_before_middle <- gaps %>%
    dplyr::filter(Lngtcm < middle_index) #  before middle point
  
  
  # Check if first_gap_before_middle has data
  if (length(first_gap_before_middle) > 0 && nrow(first_gap_before_middle) > 0) {
    first_gap_before_middle <- gaps %>%
      dplyr::filter(Lngtcm < middle_index) %>%
      dplyr::filter(Lngtcm == max(Lngtcm))
    # slice(1) # Index of first gap before middle
    
    if (!is.na(first_gap_before_middle$Lngtcm)) { 
      # Display the row where the first two consecutive zeros occur
      MinGap <- first_gap_before_middle$Lngtcm #HLTot[first_gap_before_middle$Lngtcm, ]$Lngtcm
    } else {
      message("No gap found before the middle point.")
      MinGap <- NA
    }
  } else {
    message("No data.")
    MinGap <- NA
  }
  
  dataHLOutliers <- dataHL |> dplyr::filter(Lngtmm > (MaxGap*10))
  dataHLOutliers2 <- dataHL |> dplyr::filter(Lngtmm < (MinGap*10))
  
  Outliers <- rbind(Outliers, dataHLOutliers)
  Outliers <- rbind(Outliers, dataHLOutliers2)
  
  results <- rbind(results,  data.frame(Species = unique(dataHL$ScientificName), CommonName = unique(dataHL$CommonName), TaxaGroup = TaxaInfo,
                                        SpeciesCode = unique(dataHL$Valid_Aphia), TotNo = sum(dataHL$HLNoAtLngt*dataHL$SubFactor, na.rm = T),
                                        LengthMin = min_length, LengthMax = max_length, TailGap = MaxGap, HeadGap = MinGap, 
                                        Num_Outliers = sum(dataHLOutliers$HLNoAtLngt*dataHL$SubFactor, na.rm = T) + sum(dataHLOutliers2$HLNoAtLngt*dataHL$SubFactor, na.rm = T))
  )
  
  # Save the results to an Excel file
  write.csv(results, "Length/Length_Freq_Result_2cmbins_BTS.csv")
  
  # Save the results to an Excel file
  write.csv(Outliers, "Length/DATRAS_Outliers_BTS.csv")
  
  HLTot <- dataHL %>% group_by(ScientificName, CommonName, Lngtcm) |>
    summarise(Freq_NoAtLngt = round(sum(HLNoAtLngt*SubFactor, na.rm = T),1)) |> ungroup()
  
  HLTot2 <- dataHL %>% group_by(ScientificName, CommonName, Lngtcm) |>
    summarise(Freq_NoAtLngt = round(sum(HLNoAtLngt, na.rm = T),1)) |> ungroup()
  
  
  if(nrow(HLTot) > 2 && sum(HLTot$Freq_NoAtLngt, na.rm = T)>19  & (!is.na(MaxGap) | !is.na(MinGap))){
    r <-  ggplot(HLTot, aes(x = Lngtcm, y = Freq_NoAtLngt)) +
      geom_bar(stat = "identity", fill = "#4361EE", color = "#4361EE") +
      labs(title = paste0(unique(dataHL$CommonName), " (",unique(dataHL$ScientificName),")","\n","Length Frequency" ), 
           x = "Length (cm)", y = "Frequency") +
      labs(caption = unique(dataHL$Valid_Aphia))+
      scale_y_continuous(labels = comma) +  # Format y-axis labels
      scale_fill_discrete(labels = comma) +  # Format legend labels
      theme_classic()
    if(!is.na(MaxGap)){ 
      r <-  r + geom_vline(xintercept = MaxGap, color = "red", linetype = "dashed", size = 1) +
        annotate("text", x = MaxGap, y = max(HLTot$Freq_NoAtLngt) - max(HLTot$Freq_NoAtLngt)/2, label = paste("TailGap:", round(MaxGap, 1)), size = 4, color = "red", vjust = -0.5)}
    if(!is.na(MinGap)){ r <-  r + geom_vline(xintercept = MinGap, color = "orange", linetype = "dashed", size = 1) +
      annotate("text", x = MinGap, y = max(HLTot$Freq_NoAtLngt) - max(HLTot$Freq_NoAtLngt)/2, label = paste("HeadGap:", round(MinGap, 1)), size = 4, color = "orange", vjust = -0.5)}
    
    r <- r + theme(text=element_text(size=8), #change font size of all text
                   axis.text=element_text(size=7), #change font size of axis text
                   axis.title=element_text(size=8), #change font size of axis titles
                   plot.title=element_text(size=10), #change font size of plot title
                   legend.text=element_text(size=7), #change font size of legend text
                   legend.title=element_text(size=8)) #change font size of legend title
    print(r)
    # Print the first plot on the entire first row
    # Save plot b to the list
    plot_list2[[paste(species, "1978-2024_Subfactor", sep = "_")]] <- r
  }
  
  if(nrow(HLTot2) > 2 && sum(HLTot2$Freq_NoAtLngt, na.rm = T)>19 & (!is.na(MaxGap) | !is.na(MinGap))){
    p <-  ggplot(HLTot2, aes(x = Lngtcm, y = Freq_NoAtLngt)) +
      geom_bar(stat = "identity", fill = "#4361EE", color = "#4361EE") +
      labs(title = paste0(unique(HLTot2$CommonName), " (",unique(HLTot2$ScientificName),")","\n","Length Frequency without subfactor" ), 
           x = "Length (cm)", y = "Frequency") +
      labs(caption = unique(dataHL$Valid_Aphia))+
      scale_y_continuous(labels = comma) +  # Format y-axis labels
      scale_fill_discrete(labels = comma) +  # Format legend labels
      theme_classic()
    if(!is.na(MaxGap)){ 
      p <-  p + geom_vline(xintercept = MaxGap, color = "red", linetype = "dashed", size = 1) +
        annotate("text", x = MaxGap, y = max(HLTot2$Freq_NoAtLngt) - max(HLTot2$Freq_NoAtLngt)/2, label = paste("TailGap:", round(MaxGap, 1)), size = 4, color = "red", vjust = -0.5)}
    if(!is.na(MinGap)){   p <-  p + geom_vline(xintercept = MinGap, color = "orange", linetype = "dashed", size = 1) +
      annotate("text", x = MinGap, y = max(HLTot2$Freq_NoAtLngt) - max(HLTot2$Freq_NoAtLngt)/4, label = paste("HeadGap:", round(MinGap, 1)), size = 4, color = "orange", vjust = -0.5)}
    
    p <- p + theme(text=element_text(size=8), #change font size of all text
                   axis.text=element_text(size=7), #change font size of axis text
                   axis.title=element_text(size=8), #change font size of axis titles
                   plot.title=element_text(size=10), #change font size of plot title
                   legend.text=element_text(size=7), #change font size of legend text
                   legend.title=element_text(size=8)) #change font size of legend title
    print(p)
    # Print the first plot on the entire first row
    # Save plot b to the list
    plot_list[[paste(species, "1978-2024", sep = "_")]] <- p
  }
  
  f <-  ggplot(HLTot2, aes(x = Lngtcm, y = Freq_NoAtLngt)) +
    geom_bar(stat = "identity", fill = "#4361EE", color = "#4361EE") +
    labs(title = paste0(unique(HLTot$ScientificName), " (",unique(HLTot$CommonName),")","\n","Length Frequency" ), 
         x = "Length (cm)", y = "Frequency") +
    labs(caption = paste(unique(dataHL$Valid_Aphia), TaxaInfo))+
    scale_y_continuous(labels = comma) +  # Format y-axis labels
    scale_fill_discrete(labels = comma) +  # Format legend labels
    theme_classic()
  if(!is.na(MaxGap)){ 
    f <-  f + geom_vline(xintercept = MaxGap, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = MaxGap, y = max(HLTot2$Freq_NoAtLngt) - max(HLTot2$Freq_NoAtLngt)/2, label = paste("TailGap:", round(MaxGap, 1)), size = 4, color = "red", vjust = -0.5)}
  if(!is.na(MinGap)){  
    f <-  f + geom_vline(xintercept = MinGap, color = "orange", linetype = "dashed", size = 1) +
      annotate("text", x = MinGap, y = max(HLTot2$Freq_NoAtLngt) - max(HLTot2$Freq_NoAtLngt)/4, label = paste("HeadGap:", round(MinGap, 1)), size = 4, color = "orange", vjust = -0.5)}
  
  f <- f + theme(text=element_text(size=8), #change font size of all text
                 axis.text=element_text(size=7), #change font size of axis text
                 axis.title=element_text(size=8), #change font size of axis titles
                 plot.title=element_text(size=10), #change font size of plot title
                 legend.text=element_text(size=7), #change font size of legend text
                 legend.title=element_text(size=8)) #change font size of legend title
  
  ggsave(f, filename = paste0("C:/Users/CA04/Documents/Projects/WGBEAM", "/Length/", TaxaInfo,"/", unique(HLTot2$ScientificName),"_",unique(HLTot2$CommonName), "_LengthFreq.png"))
  #print(p)
  # Print the first plot on the entire first row
  # Save plot b to the list
  plot_list3[[paste(species, "1978-2024_All", sep = "_")]] <- f
}


dev.off()

pdf("SpeciesPlots_OutlierLengths_rawBTS.pdf") 
# Save plots p (2 rows, 3 columns per page)
for (i in seq(1, length(plot_list), by = 4)) { 
  library(patchwork)
  # Fix: Use length(plot_list) instead of length(species_list) * length(year_list)
  # Get the current set of 6 plots
  current_plots <- plot_list[i:min(i + 3, length(plot_list))]
  
  # Arrange the plots in a 2-row, 3-column grid
  grid_plot <- wrap_plots(current_plots, nrow = 2, ncol = 2)
  
  # Print the grid plot to the PDF
  print(grid_plot)
}
# Close the PDF file
dev.off()

pdf("SpeciesPlots_OutlierLengths_Subfactor.pdf") 
# Save plots p (2 rows, 3 columns per page)
for (i in seq(1, length(plot_list2), by = 4)) { 
  library(patchwork)
  # Fix: Use length(plot_list) instead of length(species_list) * length(year_list)
  # Get the current set of 6 plots
  current_plots <- plot_list2[i:min(i + 3, length(plot_list2))]
  
  # Arrange the plots in a 2-row, 3-column grid
  grid_plot <- wrap_plots(current_plots, nrow = 2, ncol = 2)
  
  # Print the grid plot to the PDF
  print(grid_plot)
}
# Close the PDF file
dev.off()

pdf("SpeciesPlots_All.pdf") 
# Save plots p (2 rows, 3 columns per page)
for (i in seq(1, length(plot_list3), by = 4)) { 
  library(patchwork)
  # Fix: Use length(plot_list) instead of length(species_list) * length(year_list)
  # Get the current set of 6 plots
  current_plots <- plot_list3[i:min(i + 3, length(plot_list3))]
  
  # Arrange the plots in a 2-row, 3-column grid
  grid_plot <- wrap_plots(current_plots, nrow = 2, ncol = 2)
  
  # Print the grid plot to the PDF
  print(grid_plot)
}
# Close the PDF file
dev.off()



