get_family_species_selections <- function(state, family_selected = NULL) {
  .data <- state$data_full %>%
    dplyr::filter(
      country %in% state$country$selected,
      subnational %in% state$subnational$selected,
      local %in% state$local$selected,
      maa %in% state$maa$selected
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
