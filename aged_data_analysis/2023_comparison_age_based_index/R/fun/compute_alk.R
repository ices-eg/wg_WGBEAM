compute_alk <- function(df_datras_hh_ca,
                        age_plus_group = NULL,
                        index_grouping_var= c("Year")){

  new_age_plus_group_level <- paste0(age_plus_group, "+")
  old_age_plus_group_level <- as.character(age_plus_group)

  ### for male and female without distinction
  df_alk <- df_datras_hh_ca %>%
    ### make sure that no double rows
    distinct() %>%
    ### remove age with NA
    filter(!is.na(Age)) %>%
    # mutate(LngtClass = as.numeric(as.character(LngtClass)),
    #        LngtClass = case_when(LngtCode %in% c(".", "0") ~ LngtClass / 10,
    #                              LngtCode == "1" ~ LngtClass),
    #        LngtClass = round(LngtClass, digits = 0)) %>%
  purrr::when(
    is.null(age_plus_group) ~ .,
    ~ dplyr::mutate(., Age = ifelse(Age < age_plus_group, Age, age_plus_group))
  ) %>%

    ### only select the usefull columns
    dplyr::select(LngtClass, Age, CANoAtLngt, Year, all_of(index_grouping_var)) %>%
    group_by(LngtClass, Age, across(all_of(index_grouping_var))) %>%
    ### sum the number of aged sole per length class
    summarise(n_fish = sum(CANoAtLngt)) %>%
    arrange(Age, LngtClass) %>%
    group_by(LngtClass, across(all_of(index_grouping_var))) %>%
    mutate(total_n_fish = sum(n_fish),
           Proportion = n_fish/total_n_fish,
           Age_f = as_factor(Age),
           Age_f = fct_recode(Age_f, !!!setNames(old_age_plus_group_level, new_age_plus_group_level)),
           Age_f = fct_relevel(Age_f, !!new_age_plus_group_level, after = Inf))
  return(df_alk)
}
