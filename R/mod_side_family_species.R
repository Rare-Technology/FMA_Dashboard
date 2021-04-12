#' side_assessment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarFamUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("sel_family"),
      "Select family",
      choices = rarefma::init_family_species_selections$family,
      selected = rarefma::init_family_species_selections$family,
      multiple = TRUE,
      selectize = FALSE
    ),
    div(class = 'fam-spec-select',
    selectInput(
      ns("sel_species"),
      "Select species",
      choices = rarefma::init_family_species_selections$species,
      selected = rarefma::init_family_species_selections$species,
      multiple = TRUE,
      selectize = FALSE
    )),
    
    div(class = 'date_slider',
    sliderInput(ns("date_range"),
                label = "Select date range",
                min = min(rarefma::init_dates, na.rm = TRUE),
                max = max(rarefma::init_dates + 1, na.rm = TRUE),
                value = c(min(rarefma::init_dates, na.rm = TRUE),
                          max(rarefma::init_dates + 1, na.rm = TRUE)
                ),
                ticks = FALSE,
                timeFormat = "%F"
    )
    )
  )
}

#' side_assessment Server Function
#'
#' @noRd
sidebarFamServer <- function(id, state) {
  
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(state$maa,
                   {
                     species_info <- get_family_species_selections(
                       state$data_geo_family_species,
                       state$country$selected,
                       state$subnational$selected,
                       state$local$selected,
                       state$maa$selected,
                     )
                     
                     state$family <- list(
                       choices = species_info$family,
                       selected = species_info$family
                     )
                     
                     state$species <- list(
                       choices = species_info$species,
                       selected = species_info$species
                     )
                     
                     updateSelectInput(
                       session,
                       "sel_family",
                       choices = species_info$family,
                       selected = species_info$family
                     )
                   },
                   ignoreInit = TRUE
      )
      
      r_family <- reactive(input$sel_family)
      # d_family <- debounce(r_family, millis = 300)
      
      observeEvent(r_family(),
                   {
                     if (!setequal(r_family(), state$family$selected)) {
                       state$family$selected <- r_family()
                       species_info <- get_family_species_selections(
                         state$data_geo_family_species,
                         state$country$selected,
                         state$subnational$selected,
                         state$local$selected,
                         state$maa$selected,
                         r_family()
                       )
                       
                       if (!setequal(species_info$species, state$species$selected)) {
                         state$species$selected <- species_info$species
                         state$species$choices <- species_info$species
                       }
                       
                       
                       updateSelectInput(
                         session,
                         "sel_species",
                         choices = species_info$species,
                         selected = species_info$species
                       )
                     } else {
                       updateSelectInput(
                         session,
                         "sel_species",
                         choices = state$species$choices,
                         selected = state$species$selected
                       )
                     }
                     
                     
                     
                   },
                   ignoreInit = TRUE
      )
      
      observeEvent(input$sel_species,
                   {
                     if (!setequal(input$sel_species, state$selected$selected)) {
                       state$species$selected <- input$sel_species
                     }
                   },
                   ignoreInit = TRUE
      )
      
      
      
      
      
      observeEvent(state$species$selected, {
        

        
        data_filtered <- state$data_full %>% 
          dplyr::filter(
            country == state$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected
          )
        
        state$data_filtered <- data_filtered
        
        state$dates <- get_dates(data_filtered)
      })
      
      observeEvent(state$dates,
                   {
                     updateSliderInput(
                       session,
                       "date_range",
                       min = min(state$dates, na.rm = TRUE),
                       max = max(state$dates + 1, na.rm = TRUE),
                       value = c(min(state$dates, na.rm = TRUE),
                                 max(state$dates + 1, na.rm = TRUE)
                       ),
                     )
                   },
                   ignoreInit = TRUE
      )  
      
      
      observeEvent(input$date_range, {
        state$data_summary_filtered <- state$data_full %>% 
          dplyr::filter(
            country == state$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected,
            dplyr::between(date_2, input$date_range[1], input$date_range[2])
          ) %>% 
          create_data_summary()
        
      })
      
      observeEvent(state$data_filtered,
                   {
                     print("in data filtered")
                   },
                   ignoreInit = TRUE
      )
    }
  )


}

