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
          pickerInput(
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
          )
        )

        ui
      })
      
      observeEvent(input$sel_datasource,
        {
          cat("data source updated???")
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
          
        },
        ignoreInit = TRUE
      )
      
      observeEvent(state$resetFilters, {
        reset("sel_country")
      }, ignoreInit = TRUE)
    }
  )



  # return(
  #   sel_maa = reactive({ input$sel_maa })
  # )
}
