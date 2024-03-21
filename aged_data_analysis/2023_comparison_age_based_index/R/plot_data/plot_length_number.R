################################################################################
### plot number of fish per age and year
################################################################################

###-----------------------------------------------------------------------------
### histogram plot

### point plot
dfs_hl_year <- df_hh_hl %>%
  group_by(Year, across(all_of(index_grouping_var))) %>%
  mutate(n_fish_tot = n()) %>%
  group_by(Year, LngtClass, across(all_of(index_grouping_var))) %>%
  summarise(n_fish = n(),
            n_fish_tot = unique(n_fish_tot)) %>%
  group_by(Year) %>%
  mutate(Year = as_factor(Year),
         percentage_n_fish = n_fish / n_fish_tot * 100)


plot_hist_length_str_fyear <- ggplot(data = dfs_hl_year,
                                     aes(x = LngtClass,
                                         y = n_fish)) +
  geom_col() +
  facet_grid( ~ Year)
print(plot_hist_length_str_fyear)

ggsave(plot = plot_hist_length_str_fyear,
       filename = here("figures",
                       species_name,
                       glue("plot_hist_length_str_fyear.{device_figure}")),
       device = device_figure)


plot_length_str <- ggplot(data = df_hh_hl,
                          aes(x = LngtClass, fill = as.factor(Year))) +
  geom_histogram(stat = "count") +
  ylab("Number") +
  xlab("Length class")
print(plot_length_str)  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(plot = plot_length_str,
       filename = here("figures",
                       species_name,
                       glue("plot_length_str.{device_figure}")),
       device = device_figure)



###-----------------------------------------------------------------------------
### point plot

year_to_plot <- max(year_vec)

dfs_hl_data_year <- filter(dfs_hl_year,
                           Year == year_to_plot)
dfs_hl_data_year2 <- filter(dfs_hl_year,
                            Year == year_to_plot - 1)

plot_length_str_line <- ggplot() +
  # geom_point(data = dfs_hl_year,
  #            aes(x = LngtClass,
  #                y =  percentage_n_fish,
  #                group = Year,
  #                colour = Year)) +

  geom_line(data = dfs_hl_year,
            aes(x = LngtClass,
                y =  percentage_n_fish,
                group = Year,
                colour = Year)) +

  # geom_point(data = dfs_hl_data_year2,
  #            aes(x = LngtClass,
  #                y =  percentage_n_fish,
  #                group = Year,
  #                colour = Year)) +
  #
  # geom_line(data = dfs_hl_data_year2,
  #           aes(x = LngtClass,
  #               y =  percentage_n_fish,
  #               group = Year,
  #               colour = Year),
  #           size = 2,
  #           alpha = 0.5) +

  geom_point(data = dfs_hl_data_year,
             aes(x = LngtClass,
                 y =  percentage_n_fish,
                 group = Year,
                 colour = Year)) +
  geom_line(data = dfs_hl_data_year,
            aes(x = LngtClass,
                y =  percentage_n_fish,
                group = Year,
                colour = Year),
            size = 2.5,
            alpha = 0.5) +
  ylab("Number percentage") +
  xlab("Length class") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~.data[[index_grouping_var]], ncol = 2)
print(plot_length_str_line)

ggsave(plot = plot_length_str_line,
       filename = here("figures",
                       species_name,
                       glue("plot_length_str_line.{device_figure}")),
       device = device_figure)

