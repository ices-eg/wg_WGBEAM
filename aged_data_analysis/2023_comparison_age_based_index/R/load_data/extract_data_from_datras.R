###-----------------------------------------------------------------------------
### create path for saving data
dir.create(paste0("data/tidy/",
                  survey_code,
                  "/",
                  species_name, "/"),
           showWarnings = FALSE,
           recursive = TRUE)
dir.create(paste0("data/figures/", survey_code, "/", species_name, "/"),
           showWarnings = FALSE,
           recursive = TRUE)

################################################################################
### Extract data from datras website
###############################################################################
if (file.exists(paste0("data/tidy/", species_name,
                       "/datatras_",
                       min(year_vec), "_", max(year_vec),".rds")) &
    erase_tidy_data == FALSE) {
  cat("Load datras data already saved \n")
  load(file = paste0("data/tidy/", species_name, "/datatras_",
                     min(year_vec), "_", max(year_vec),".rds"))
  unpack_list(list_datras)
} else {

  cat("Generate datras data and save it \n")

  ###---------------------------------------------------------------------------
  ### Load haul data
  cat("Load haul data in df_hh \n")
  df_hh <- icesDatras::getDATRAS(record = "HH",
                                 survey = survey_code,
                                 years = year_vec,
                                 quarters = quarter_vec) %>%
    as_tibble() %>%
    drop_NA_cols_for_loop(.) %>%
    drop_9_cols_for_loop(.) %>%
    filter(HaulVal == "V") %>%
    distinct() %>%
    mutate(hh_id_row = row_number(),
           hh_id = glue("{Survey}-{Year}-{Quarter}-{Country}-{Gear}-{Ship}-{StNo}-{HaulNo}"),
           beamwidth = case_when(Gear == "BT8" ~ 8,
                                 Gear == "BT7" ~ 7,
                                 Gear == "BT6" ~ 6,
                                 Gear == "BT4A" ~ 4,
                                 Gear == "BT4AI" ~ 4,
                                 Gear == "BT4S" ~ 4,
                                 Gear == "BT3" ~ 3,
                                 Gear == "BT4P" ~ 4,
                                 TRUE ~ NA_real_))
  ### Compute distance form Lat/Lon
  #Measuring Distance
  get_dist <- function(x, df){
    round(distm(x = c(df$ShootLong[x],df$ShootLat[x]),
                y = c(df$HaulLong[x], df$HaulLat[x])),
          0)
  }

  df_hh$TowDistance <- pbsapply(1:nrow(df_hh), get_dist,
                                df = df_hh)
  if (survey_code == "BTS-VIII") {
    df_hh <- df_hh %>%
      mutate(swept_area_km2_dist = ((beamwidth)*(TowDistance))/1000000)
  } else {
    df_hh <- df_hh %>%
      mutate(swept_area_km2 = ((beamwidth/1000)*(1.852*GroundSpeed*HaulDur/60)),
             swept_area_km2_dist = ((beamwidth)*(TowDistance))/1000000)
  }

  ###---------------------------------------------------------------------------
  ### Load haul data
  cat("Load length data in df_hl \n")
  df_hl <- icesDatras::getDATRAS(record = "HL",
                                 survey = survey_code,
                                 years = year_vec,
                                 quarters = quarter_vec) %>%
    as_tibble() %>%
    drop_NA_cols_for_loop(.) %>%
    drop_9_cols_for_loop(.) %>%
    filter(SpecCode == species_code,
           HaulNo %in% df_hh$HaulNo) %>%
    mutate(LngtClass = as_factor(LngtClass),
           LngtClass = as.numeric(as.character(LngtClass)),
           LngtClass = case_when(LngtCode == "." ~ LngtClass / 10,
                                 LngtCode == "0" ~ floor(LngtClass / 10),
                                 LngtCode == "2" ~ LngtClass,
                                 LngtCode == "5" ~ LngtClass,
                                 LngtCode == "1" ~ LngtClass),
           LngtClass = round(LngtClass, digits = 0),
           hh_id = glue("{Survey}-{Year}-{Quarter}-{Country}-{Gear}-{Ship}-{StNo}-{HaulNo}"))  %>%
    filter(!is.na(LngtClass))

  ###---------------------------------------------------------------------------
  ### Load CA data
  cat("Load age data in df_ca \n")
  df_ca <- icesDatras::getDATRAS(record = "CA",
                                 survey = survey_code,
                                 years = year_vec,
                                 quarters = quarter_vec) %>%
    as_tibble() %>%
    drop_NA_cols_for_loop(.) %>%
    drop_9_cols_for_loop(.)  %>%
    filter(SpecCode == species_code,
           HaulNo %in% df_hh$HaulNo) %>%
    mutate(LngtClass = as_factor(LngtClass),
           LngtClass = as.numeric(as.character(LngtClass)),
           LngtClass = case_when(LngtCode == "." ~ LngtClass / 10,
                                 LngtCode == "0" ~ floor(LngtClass / 10),
                                 LngtCode == "5" ~ LngtClass,
                                 LngtCode == "2" ~ LngtClass,
                                 LngtCode == "1" ~ LngtClass),
           LngtClass = round(LngtClass, digits = 0),
           id_row = row_number(),
           hh_id = glue("{Survey}-{Year}-{Quarter}-{Country}-{Gear}-{Ship}-{StNo}-{HaulNo}")) %>%
    filter(!is.na(LngtClass))

  ###---------------------------------------------------------------------------
  ### Merge tables for age data and haul data
  df_hh_ca <- left_join(df_ca, select(df_hh, -any_of(c("RecordType", "Survey",
                                                "Quarter", "Country",
                                                "Ship", "Gear",
                                                "GearEx", "StNo",
                                                "HaulNo", "Year",
                                                "DateofCalculation"))),
                        by = "hh_id" ) %>%
    arrange(HaulNo, StNo) %>%
    mutate_if(is.character, as.factor)

  ###---------------------------------------------------------------------------
  ### Merge tables for length data and haul data
  ### Load haul data
  df_hh_hl <- left_join(df_hl, select(df_hh, -any_of(c("RecordType", "Survey",
                                                "Quarter", "Country",
                                                "Ship", "Gear",
                                                "GearEx", "StNo",
                                                "HaulNo", "Year",
                                                "DateofCalculation"))),
                        by = "hh_id" ) %>%
    arrange(HaulNo, StNo) %>%
    mutate_if(is.character, as.factor)

  ###---------------------------------------------------------------------------
  ### Save data
  list_datras <- list(df_hh = df_hh,
                      df_hl = df_hl,
                      df_ca = df_ca,
                      df_hh_ca = df_hh_ca,
                      df_hh_hl = df_hh_hl)

  save(list_datras, file = paste0("data/tidy/",
                                  survey_code, "/",
                                  species_name,
                                  "/datatras_",
                                  min(year_vec),
                                  "_", max(year_vec),".rds"))
}
