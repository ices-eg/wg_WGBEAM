################################################################################
### Plot df_alk_prop_female
################################################################################

if(length(index_grouping_var) == 1){
  facet_formula <- glue("ScientificName ~ {index_grouping_var}")
} else {
  if ("ScientificName" %in% index_grouping_var) {
    index_grouping_var_temp <-  index_grouping_var[index_grouping_var != "ScientificName"]
    facet_formula <- glue("ScientificName ~ {paste(index_grouping_var_temp, collapse = '+')}")
  } else {
    facet_formula <- glue("ScientificName ~ {paste(index_grouping_var, collapse = '+')}")
  }
}

plot_alk_prop <- ggplot() +
  geom_bar(data = df_alk_prop_long,
           aes(x = LngtClass, y = Proportion, fill = Age_f ),
           stat = "identity") +
  facet_grid(as.formula(facet_formula))  +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name = "Age")
print(plot_alk_prop)

ggsave(plot = plot_alk_prop,
       filename = here(glue("{path_figures}/plot_alk_prop.{device_figure}")),
       device = device_figure)
