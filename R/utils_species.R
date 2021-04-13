get_family_species_selections <- function(.data,
                                   country_selected,
                                    subnational_selected,
                                   local_selected,
                                   maa_selected,
                                   dates,
                                   family_selected = NULL
                                   ){
 

  .data <- .data %>% 
    dplyr::filter(
      country %in% country_selected,
      subnational %in% subnational_selected,
      local %in% local_selected,
      maa %in% maa_selected,
      dplyr::between(transaction_date, dates[1], dates[2])
    ) %>% 
    dplyr::select(
      family,
      species
    ) %>% 
    dplyr::distinct() 
    
  print(length(sort(unique(.data$family))))
  print(length(sort(unique(.data$species))))    
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