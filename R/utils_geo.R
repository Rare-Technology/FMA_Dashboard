


get_country_selections <- function(.data){

  
    choices <- .data %>% 
      dplyr::pull(country) %>% 
      unique() %>% 
      as.character() %>% 
      sort()
  
  
    selected <- choices[1]
  
    list(choices = choices, selected = selected)
}


get_subnational_selections <- function(.data, country_selected){
  choices <- .data %>% 
    dplyr::filter(country %in% country_selected) %>% 
    dplyr::pull(subnational) %>% 
    unique()
    as.character() %>% 
    sort()
  
  list(choices = choices, selected = choices)
}


get_local_selections <- function(.data, country_selected, subnational_selected){

  choices <- .data %>% 
    dplyr::filter(country %in% country_selected) %>% 
    dplyr::filter(subnational %in% subnational_selected) %>% 
    dplyr::pull(local) %>% 
    unique() %>% 
    as.character() %>% 
    sort()
  
  list(choices = choices, selected = choices)
}


get_maa_selections <- function(.data, 
                               country_selected, 
                               subnational_selected,
                               local_selected){

  choices <- .data %>% 
    dplyr::filter(country %in% country_selected) %>% 
    dplyr::filter(subnational %in% subnational_selected) %>% 
    dplyr::filter(local %in% local_selected) %>% 
    dplyr::pull(maa) %>% 
    unique() %>% 
    as.character() %>% 
    sort()
  
  list(choices = choices, selected = choices)
}


get_geo_selections <- function(.data, 
                               country_choices = NULL, 
                               country_selected = NULL,
                               subnational_choices = NULL, 
                               subnational_selected = NULL, 
                               local_choices = NULL, 
                               local_selected = NULL, 
                               maa_choices = NULL, 
                               maa_selected = NULL
){
  
  if(is.null(country_choices)){
    country_choices <- .data %>% 
      dplyr::pull(country) %>% 
      unique() %>% 
      as.character() %>% 
      sort()
  }
  
  if(is.null(country_selected)){
    country_selected <- country_choices[1]
  }
  
  if(is.null(subnational_choices)){
    subnational_choices <- .data %>% 
      dplyr::filter(country %in% country_selected) %>% 
      dplyr::pull(subnational) %>% 
      unique() %>% 
      as.character() %>% 
      sort()
  }
  
  if(is.null(subnational_selected)){
    subnational_selected <- subnational_choices
  }
  
  
  
  if(is.null(local_choices)){
    local_choices <- .data %>% 
      dplyr::filter(country %in% country_selected) %>% 
      dplyr::filter(subnational %in% subnational_selected) %>% 
      dplyr::pull(local) %>% 
      unique() %>% 
      as.character() %>% 
      sort()
  }
  
  if(is.null(local_selected)){
    local_selected <- local_choices
  }
  
  
  if(is.null(maa_choices)){
    maa_choices <- .data %>% 
      dplyr::filter(country %in% country_selected) %>% 
      dplyr::filter(subnational %in% subnational_selected) %>% 
      dplyr::filter(local %in% local_selected) %>% 
      dplyr::pull(maa) %>% 
      unique() %>% 
      as.character() %>% 
      sort()
  }
  
  if(is.null(maa_selected)){
    maa_selected <- maa_choices
  }
  
  
  
  list(
    country = list(choices = country_choices, selected = country_selected),
    subnational = list(choices = subnational_choices, selected = subnational_selected),
    local = list(choices = local_choices, selected = local_selected),
    maa = list(choices = maa_choices, selected = maa_selected)
  )
}

