get_family_species_selections <- function(.data,
                                          country_selected,
                                          subnational_selected,
                                          local_selected,
                                          maa_selected,
                                          #dates,
                                          family_selected = NULL) {
  .data <- .data %>%
    dplyr::filter(
      country %in% country_selected,
      subnational %in% subnational_selected,
      local %in% local_selected,
      maa %in% maa_selected#,
      #dplyr::between(transaction_date, dates$valmin, dates$valmax)
    ) %>%
    dplyr::select(
      family,
      species
    ) %>%
    dplyr::distinct()

  if (is.null(family_selected)) {
    return(list(
      family = sort(unique(.data$family)),
      species = sort(.data$species)
    ))
  }

  .data <- .data %>%
    dplyr::filter(
      family %in% family_selected
    )

  return(list(
    family = sort(unique(.data$family)),
    species = sort(.data$species)
  ))
}

get_species_counts <- function(state) {
  out <- state$data_full %>% 
    dplyr::filter(
      maa %in% state$maa$selected,
      family %in% state$family$selected
    ) %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarize(count = sum(count, na.rm = TRUE)) %>%
    dplyr::arrange(species)
  
  return(out)
}


# fma_data_raw$ourfish$data %>%
#   dplyr::filter(
#     country == "Indonesia"
#   ) %>%
#   dplyr::group_by(species) %>%
#   dplyr::summarize(count = sum(count)) %>%
#   dplyr::arrange(species) %>% View