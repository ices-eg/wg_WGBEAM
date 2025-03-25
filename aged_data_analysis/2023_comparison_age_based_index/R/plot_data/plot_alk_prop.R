################################################################################
### Plot df_alk_prop_female
################################################################################

plot_alk_prop <- ggplot() +
  geom_bar(data = df_alk_prop_long,
           aes(x = LngtClass, y = Proportion, fill = Age_f ),
           stat = "identity") +
  facet_grid(as.formula(glue("{index_grouping_var[2]} ~ Year")))  +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name = "Age") +
  xlim(0, 30)
print(plot_alk_prop)

ggsave(plot = plot_alk_prop,
       filename = here("figures",
                       species_name,
                       glue("plot_alk_prop.{device_figure}")),
       device = device_figure)
