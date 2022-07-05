#' side_assessment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
sidebarStockUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stock_out"))
  )
}

#' side_assessment Server Function
#'
#' @noRd
sidebarStockServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$stock_out <- renderUI({
        ui <- list()
        
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
      
      observeEvent(state$maa$selected,
        # Update fish stock options/selections to reflect the selected regions.
        {
          family_species_info <- get_family_species_selections(state)

          state$family <- list(
            choices = family_species_info$family,
            selected = family_species_info$family
          )

          state$species <- list(
            choices = family_species_info$species,
            selected = family_species_info$species
          )

          updatePickerInput(
            session,
            "sel_family",
            choices = family_species_info$family,
            selected = family_species_info$family
          )
        },
        ignoreInit = TRUE
      )

      observeEvent(input$sel_family,
        {
          if (!setequal(input$sel_family, state$family$selected)) {
            state$family$selected <- input$sel_family
            species_info <- get_family_species_selections(state, input$sel_family)
    
            if (!setequal(species_info$species, state$species$selected)) {
              state$species$selected <- species_info$species
              state$species$choices <- species_info$species
            }

            updatePickerInput(
              session,
              "sel_species",
              choices = species_info$species,
              selected = species_info$species
            )
          } else {
            updatePickerInput(
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
    }
  )
}
