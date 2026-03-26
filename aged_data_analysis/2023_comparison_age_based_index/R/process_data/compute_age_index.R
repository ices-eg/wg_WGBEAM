################################################################################
### Compute for HL the numbers of individuals per Km2
################################################################################

### Compute ALK
df_alk_prop_index <- compute_alk(df_hh_ca,
                                 age_plus_group = max_age,
                                 index_grouping_var = NULL
) %>%
  mutate(LngtClass = factor(LngtClass)) %>%
  select(LngtClass, Age, Proportion, ScientificName) %>%
  pivot_wider(id_cols = c(ScientificName, LngtClass),
              names_from =Age,
              values_from = Proportion,
              names_prefix =  "age",
              values_fill = 0)


### Compute n fish in HL with a  Length class
df_hh_hl_to_age <- df_hh_hl %>%
  ungroup() %>%
  filter(LngtClass != "(Missing)") %>%
  mutate(LngtClass = factor(LngtClass)) %>%
  group_by(hh_id, LngtClass, Year, swept_area_km2_dist) %>%
  summarise(HLNoAtLngt = sum(HLNoAtLngt * SubFactor),
            .groups = "keep")

### Compute abundance at age
df_hh_hl_age <- left_join(df_hh_hl_to_age,
                          df_alk_prop_index,
                          by = join_by(LngtClass)) %>%
  mutate(across(starts_with("age"), ~ . * HLNoAtLngt)) %>%
  pivot_longer(starts_with("age"),
               names_to = "Age",
               values_to = "Abundance") %>%
  mutate(Age = as.numeric(str_sub(string = Age,
                                  start = 4,
                                  end = 4)))

### Compute average abundance at age and year per km2
dfs_hh_hl_age <- df_hh_hl_age %>%
  group_by(hh_id, Year, Age, swept_area_km2_dist) %>%
  summarise(n_fish_tot = sum(Abundance)) %>%
  group_by(Year, Age) %>%
  mutate(n_fish_per_km2 = n_fish_tot/swept_area_km2_dist) %>%
  group_by(Year, Age) %>%
  summarise(avg_fish_per_km2 = mean(n_fish_per_km2, na.rm = TRUE)) %>%
  mutate(Year = as_factor(Year),
         Age_f        = as.numeric(Age),                    # numeric first
         Age_f        = factor(Age_f, ordered = TRUE),      # ordered numeric factor
         Age_f        = forcats::fct_recode(Age_f, !!!setNames(as.character(max_age), glue::glue("{max_age}+"))),
         Age_f        = forcats::fct_relevel(Age_f, sort(levels(Age_f)), after = Inf)
  )



plot_avg_abundance_km2 <- ggplot() +
  geom_col(data = dfs_hh_hl_age,
           aes(x = Year, y = avg_fish_per_km2)) +
  facet_wrap(~Age_f, scales = "free") +
  theme_bw() +
  ylab("Average number per km2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  ggsave(plot = plot_avg_abundance_km2,
         filename = here("figures",
                         species_name,
                         glue("plot_avg_abundance_km2.{device_figure}")),
         device = device_figure)

