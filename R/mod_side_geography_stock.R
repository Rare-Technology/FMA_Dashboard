#' side_geography UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom shinyjs reset
sidebarGeoUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("geo_out"))
}

#' side_geography Server Function
#'
#' @noRd
sidebarGeoServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$geo_out <- renderUI({
        ui <- list(
          selectInput(
            ns("sel_datasource"),
            tr(state, "Data source"),
            choices = fma_data_sources,
            selected = fma_init_data_source,
          ),
          div(class='sidetitle', tr(state, "Geography")),
          selectInput(
            ns("sel_country"),
            tr(state, "Country"),
            choices = fma_init_geo_selections$country$choices,
            selected = fma_init_geo_selections$country$selected
          ),
          pickerInput(
            ns("sel_subnational"),
            tr(state, "Subnational unit"),
            choices = fma_init_geo_selections$subnational$choices,
            selected = fma_init_geo_selections$subnational$selected,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2",
              `count-selected-text` = paste("{0}", tr(state, "items selected")),
              `none-selected-text` = tr(state, "Nothing selected"),
              `live-search` = TRUE
            )
          ),
          pickerInput(
            ns("sel_local"),
            tr(state, "Local government unit"),
            choices = fma_init_geo_selections$local$choices,
            selected = fma_init_geo_selections$local$selected,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2",
              `count-selected-text` = paste("{0}", tr(state, "items selected")),
              `none-selected-text` = tr(state, "Nothing selected"),
              `live-search` = TRUE
            )
          ),
          pickerInput(
            ns("sel_maa"),
            tr(state, "Managed access area"),
            choices = fma_init_geo_selections$maa$choices,
            selected = fma_init_geo_selections$maa$choices,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2",
              `count-selected-text` = paste("{0}", tr(state, "items selected")),
              `none-selected-text` = tr(state, "Nothing selected"),
              `live-search` = TRUE
            )
          ),
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
      
      observeEvent(input$sel_datasource,
        {
          state$data_source <- input$sel_datasource
          state$data_full <- fma_data_raw[[input$sel_datasource]]$data
          state$data_geo_family_species <- fma_data_geo_family_species[[input$sel_datasource]]
          state$data_summary <- fma_data_summary[[input$sel_datasource]]
          country_info <- get_country_selections(state$data_geo_family_species)
          state$country <- list(
            choices = country_info$choices,
            selected = country_info$selected
          )
          updateSelectInput(
            session,
            "sel_country",
            choices = country_info$choices,
            selected = country_info$selected
          )
        },
        ignoreInit = TRUE, ignoreNULL = TRUE
      )

      observeEvent(input$sel_country,
        {
          if (input$sel_country != state$country$selected) {
            state$country$selected <- input$sel_country
          }

          subnational_info <- get_subnational_selections(
            state$data_geo_family_species,
            country_selected = input$sel_country
          )
          state$subnational <- list(
            choices = subnational_info$choices,
            selected = subnational_info$selected
          )
          updatePickerInput(
            session,
            "sel_subnational",
            choices = subnational_info$choices,
            selected = subnational_info$selected
          )
        },
        ignoreInit = TRUE
      )


      observeEvent(input$sel_subnational,
        {
          # if (!setequal(input$sel_subnational, state$subnational$selected)) {
          state$subnational$selected <- input$sel_subnational
          # }

          local_info <- get_local_selections(state$data_geo_family_species,
            country_selected = input$sel_country,
            subnational_selected = input$sel_subnational
          )
          state$local <- list(
            choices = local_info$choices,
            selected = local_info$selected
          )
          updatePickerInput(
            session,
            "sel_local",
            choices = local_info$choices,
            selected = local_info$selected
          )
        },
        ignoreInit = TRUE
      )

      observeEvent(input$sel_local,
        {
          # if (!setequal(input$sel_local, state$local$selected)) {
          state$local$selected <- input$sel_local
          # }

          maa_info <- get_maa_selections(
            state$data_geo_family_species,
            country_selected = input$sel_country,
            subnational_selected = input$sel_subnational,
            local_selected = input$sel_local
          )
          state$maa <- list(
            choices = maa_info$choices,
            selected = maa_info$selected
          )
          updatePickerInput(
            session,
            "sel_maa",
            choices = maa_info$choices,
            selected = NULL
          )
        },
        ignoreInit = TRUE
      )


      observeEvent(input$sel_maa,
        {
          # if (!setequal(input$sel_maa, state$maa$selected)) {
          state$maa$selected <- input$sel_maa
          # }
          data_filtered <- state$data_full %>%
            dplyr::filter(
              country %in% input$sel_country,
              subnational %in% input$sel_subnational,
              local %in% input$sel_local,
              maa %in% input$sel_maa
            )
          
          
          state$data_filtered <- data_filtered
          state$data_summary_filtered <- data_filtered %>%
            create_data_summary()
          
          species_info <- get_family_species_selections(
            state$data_full,
            input$sel_country,
            input$sel_subnational,
            input$sel_local,
            input$sel_maa,
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
      
      observeEvent(input$sel_family,
        {
         if (!setequal(input$sel_family, state$family$selected)) {
           # User changes family selections directly
           state$family$selected <- input$sel_family
         }
         # Otherwise, family selections were changed due to maa selections changing.
         # In this case, input$sel_family and state$family are already in sync
         species_info <- get_family_species_selections(
           state$data_full,
           input$sel_country,
           input$sel_subnational,
           input$sel_local,
           input$sel_maa,
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
        },
        ignoreInit = TRUE, ignoreNULL = FALSE
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
        current_dates <- get_dates(data_filtered)
        
        
        state$current_date_range <- list(
          min = min(current_dates, na.rm = TRUE),
          max = max(current_dates, na.rm = TRUE),
          valmin = min(current_dates, na.rm = TRUE),
          valmax = max(current_dates, na.rm = TRUE)
        ) 
      })
      
      observeEvent(state$resetFilters, {
        reset("sel_country")
      }, ignoreInit = TRUE)
    }
  )
}
