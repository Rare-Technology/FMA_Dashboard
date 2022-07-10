#' side_stock UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
sidebarStockUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("stock_out"))
}
    
#' side_stock Server Functions
#'
#' @noRd 
sidebarStockServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$stock_out <- renderUI({
        ui <- list(
          div(class='sidetitle', tr(state, "Stock")),
          pickerInput(
            ns("sel_family"),
            tr(state, "Family"),
            choices = fma_init_family_species_selections$family,
            selected = fma_init_family_species_selections$family,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 1",
              `count-selected-text` = paste("{0}", tr(state, "items selected")),
              `none-selected-text` = tr(state, "Nothing selected"),
              `live-search` = TRUE
            )
          ),
          div(
            class = "fam-spec-select",
            pickerInput(
              ns("sel_species"),
              tr(state, "Species"),
              choices = fma_init_family_species_selections$species,
              selected = fma_init_family_species_selections$species,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = paste("{0}", tr(state, "items selected")),
                `none-selected-text` = tr(state, "Nothing selected"),
                `live-search` = TRUE
              )
            )
          )
        )
        
        ui
      })
      
      
      observeEvent(state$maa, {
         species_info <- get_family_species_selections(
           state$data_full,
           state$country$selected,
           state$subnational$selected,
           state$local$selected,
           state$maa$selected,
         )
         state$family <- list(
           choices = species_info$family,
           selected = species_info$family
         )
         updatePickerInput(
           session,
           "sel_family",
           choices = species_info$family,
           selected = species_info$family
         )
        },
        ignoreInit = TRUE, ignoreNULL = FALSE
      )
      
      observeEvent(input$sel_family, {
       if (!setequal(input$sel_family, state$family$selected)) {
         # User changes family selections directly
         state$family$selected <- input$sel_family
       }
       # Otherwise, family selections were changed due to maa selections changing.
       # In this case, input$sel_family and state$family are already in sync
       species_info <- get_family_species_selections(
         state$data_full,
         state$country$selected,
         state$subnational$selected,
         state$local$selected,
         state$maa$selected,
         input$sel_family
       )
       
       if (!setequal(species_info$species, state$species$selected)) {
         state$species$selected <- species_info$species
         state$species$choices <- species_info$species
       }
       
       species_counts <- get_species_counts(state)
       
       updatePickerInput(
         session,
         "sel_species",
         choices = species_info$species,
         choicesOpt = list(
           subtext = paste("Count", species_counts$count, sep = ": ")
         ),
         selected = species_info$species
       )
      }, ignoreInit = TRUE, ignoreNULL = FALSE
      )
      
      observeEvent(input$sel_species, {
        if (!setequal(input$sel_species, state$selected$selected)) {
         state$species$selected <- input$sel_species
        }
      }, ignoreInit = TRUE
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
        current_dates <- get_dates(data_filtered)
        
        
        state$current_date_range <- list(
          min = min(current_dates, na.rm = TRUE),
          max = max(current_dates, na.rm = TRUE),
          valmin = min(current_dates, na.rm = TRUE),
          valmax = max(current_dates, na.rm = TRUE)
        ) 
      })
    }
  )
}
