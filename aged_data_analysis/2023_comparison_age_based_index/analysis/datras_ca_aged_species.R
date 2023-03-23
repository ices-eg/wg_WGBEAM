################################################################################
### Aged species : extract SpecCode of CA table
################################################################################

###-----------------------------------------------------------------------------
### erase data already computed
erase_tidy_data <- FALSE

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
###-----------------------------------------------------------------------------
### Load CA data
cat("Load age data in df_ca \n")
df_datras_ca <- icesDatras::getDATRAS(record = "CA",
                                      survey = survey_code,
                                      years = year_vec,
                                      quarters = quarter_vec)

### Factor and SpecCode as age
df_ca <- df_datras_ca %>%
  as_tibble() %>%
  drop_NA_cols_for_loop(.) %>%
  drop_9_cols_for_loop(.)  %>%
  mutate(SpecCode = as_factor(SpecCode),
         Age = ifelse(is.na(Age), "Missing", Age),
         Age = as_factor(Age),
         id_row = row_number(),
         LngtClass = as_factor(LngtClass),
         LngtClass = as.numeric(as.character(LngtClass)),
         LngtClass = case_when(LngtCode == "." ~ LngtClass / 10,
                               LngtCode == "0" ~ floor(LngtClass / 10),
                               LngtCode == "1" ~ LngtClass),
         LngtClass = round(LngtClass, digits = 0))

### Summary ca table
dfs_ca_species <- df_ca %>%
  filter(Age != "Missing") %>%
  droplevels() %>%
  group_by(Year, Country, SpecCode) %>%
  summarise(n_aged_fish = n())

### get scientific names from worms data base with worms package
df_scientific_names <- worms::wormsbyid(as.numeric(levels(dfs_ca_species$SpecCode))) %>%
  select(AphiaID, scientificname) %>%
  mutate(AphiaID = as_factor(AphiaID)) %>%
  rename(ScientificName = scientificname)

dfs_ca_species <- left_join(dfs_ca_species, df_scientific_names,
                            by = c("SpecCode" = "AphiaID"))

### plot data
plot_ca_species <- ggplot(data = dfs_ca_species,
                          aes(y = ScientificName ,
                              x = n_aged_fish, fill = Country)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_grid(~Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.45)) +
  ylab("Species") + xlab('Number of aged fish')
plot_ca_species

ggsave(plot_ca_species,
       filename = here("figures",
                       glue("plot_ca_species.{device_figure}")),
       device = device_figure)

### plot data for the most aged species (at least 300 per country on annual average)
dfs_ca_species_n300 <- dfs_ca_species %>%
  group_by(Country, SpecCode) %>%
  mutate(average_year_n_aged_fish = mean(n_aged_fish)) %>%
  filter(average_year_n_aged_fish >= 300) %>%
  droplevels()

plot_ca_species_n300 <- ggplot(data = dfs_ca_species_n300,
                               aes(y = ScientificName ,
                                   x = n_aged_fish, fill = Country)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_grid(~Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.45)) +
  ylab("Species") + xlab('Number of aged fish')
plot_ca_species_n300

ggsave(plot_ca_species_n300,
       filename = here("figures",
                       glue("plot_ca_species_n300.{device_figure}")),
       device = device_figure)
