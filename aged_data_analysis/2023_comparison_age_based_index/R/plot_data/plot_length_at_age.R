################################################################################
### Plot length at age at length from CA table
################################################################################

### Uncount CA table to be sure to have one fish per row
df_ca_uncount <- df_ca %>%
  uncount(CANoAtLngt) %>%
  mutate(Age = as_factor(Age))

plot_length_at_age_boxplot <- ggplot(data = df_ca_uncount,
                                     aes(x = Age,
                                         y = LngtClass,
                                         fill = Age)) +
  geom_boxplot() +
  ylab("Numbers of fish") +
  xlab("Length class (cm)")

print(plot_length_at_age_boxplot)

ggsave(plot = plot_length_at_age_boxplot,
       filename = here(glue("{path_figures}/plot_length_at_age_boxplot.{device_figure}")),
       device = device_figure)

### summarize CA table for computing number of fish per age and length
dfs_ca_summary <- df_ca %>%
  filter(!is.na(Age)) %>%
  group_by(LngtClass, Age, hh_id, Year) %>%
  summarise(n_fish = sum(CANoAtLngt, na.rm = TRUE)) %>%
  mutate(Age = as_factor(Age))

plot_length_at_age_dist <- ggplot(data = dfs_ca_summary,
                                  aes(x = LngtClass,
                                      y = n_fish,
                                      fill = Age)) +
  geom_col() +
  ylab("Numbers of fish") +
  xlab("Length class (cm)")

print(plot_length_at_age_dist)

ggsave(plot = plot_length_at_age_dist,
       filename = here(glue("{path_figures}/plot_length_at_age_dist.{device_figure}")),
       device = device_figure)

