get_family_species_selections <- function(.data,
                                   country_selected,
                                    subnational_selected,
                                   local_selected,
                                   maa_selected,
                                   family_selected = NULL
                                   ){
 
  .data <- .data %>% 
    dplyr::filter(
      country %in% country_selected,
      subnational %in% subnational_selected,
      local %in% local_selected,
      maa %in% maa_selected
    ) %>% 
    dplyr::select(
      family,
      species
    ) %>% 
    dplyr::distinct() 
    
  
  if(is.null(family_selected))
    return(list(family = sort(unique(.data$family)), 
                species = sort(.data$species)))
  
  .data <- .data %>% 
    dplyr::filter(
      family %in% family_selected
    ) 
  
  return(list(family = sort(unique(.data$family)), 
              species = sort(.data$species)))

  
}