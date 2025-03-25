compute_alk <- function(df_datras_hh_ca,
                        age_plus_group = NULL,
                        index_grouping_var = c("Year")) {

  new_age_plus_group_level <- paste0(age_plus_group, "+")
  old_age_plus_group_level <- as.character(age_plus_group)

  ### for male and female without distinction
  df_alk <- df_datras_hh_ca %>%
    ### make sure that no double rows
    dplyr::distinct() %>%
    ### remove age with NA
    dplyr::filter(!is.na(Age))

  # Conditional mutate based on age_plus_group
  if (!is.null(age_plus_group)) {
    df_alk <- df_alk %>%
      dplyr::mutate(Age = ifelse(Age < age_plus_group, Age, age_plus_group))
  }

  df_alk <- df_alk %>%
    ### only select the useful columns
    dplyr::select(LngtClass, Age, CANoAtLngt, Year, ScientificName, all_of(index_grouping_var)) %>%
    dplyr::group_by(LngtClass, Age, ScientificName, across(all_of(index_grouping_var))) %>%
    ### sum the number of aged sole per length class
    dplyr::summarise(n_fish = sum(CANoAtLngt)) %>%
    dplyr::arrange(Age, LngtClass) %>%
    dplyr::group_by(LngtClass, ScientificName, across(all_of(index_grouping_var))) %>%
    dplyr::mutate(total_n_fish = sum(n_fish),
                  Proportion = n_fish / total_n_fish,
                  Age_f = forcats::as_factor(Age),
                  Age_f = forcats::fct_recode(Age_f, !!!setNames(old_age_plus_group_level, new_age_plus_group_level)),
                  Age_f = forcats::fct_relevel(Age_f, !!new_age_plus_group_level, after = Inf))

  return(df_alk)
}
