#'  performance indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarIndicatorUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("performance_indicators"))
  )
}

#' sidebar performance indicators Server Function
#'
#' @noRd
sidebarIndicatorServer <- function(id, state) {
  ns <- NS(id)


  moduleServer(
    id,
    function(input, output, session) {

      # Dates slider

      output$performance_indicators <- renderUI({
        tab <- state$current_tab
        ui <- list()



        # ---- Select, Visualize, Interpret tabs
        if (tab %in% c("Select", "Visualize", "Interpret")) {
          ui[["performance_indicators"]] <- div(class = " pi_widget", selectInput(
            inputId = ns("performance_indicators"),
            label = "Select performance indicators",
            choices = c(
              "Fishing Gear",
              "Reporting Effort",
              "Species Composition",
              # "Fished:Unfished Ratio",
              "Average Length",
              "Average Trophic Level",
              #' Spawning Potential Ratio',
              "Size Structure",
              "Size Proportions",
              "CPUE",
              "Total Landings"
            ),
            selected = state$current_indicator
          ))
        }

        # ---- Data tab
        if (tab %in% c("Data", "Visualize")) {
          ui[["date_slider"]] <- div(
            class = "date_slider pi_widget",
            sliderInput(ns("date_range"),
              label = "Select date range",
              min = min(init_dates, na.rm = TRUE),
              max = max(init_dates + 1, na.rm = TRUE),
              value = c(
                min(init_dates, na.rm = TRUE),
                max(init_dates + 1, na.rm = TRUE)
              ),
              ticks = FALSE,
              timeFormat = "%F"
            )
          )
        }



        if (tab %in% c("Visualize")) {
          ui[["loess_span"]] <- div(
            class = "smooth_slider pi_widget",
            sliderInput(
              inputId = ns("loess_span"),
              label = "Change curve smoothing",
              min = 0.1, max = 1,
              value = 0.5,
              step = 0.1,
              ticks = FALSE
            )
          )
        }

        ui
      })

      observeEvent(state$dates,
        {
          # updateSliderInput(
          #   session,
          #   "date_range",
          #   min = min(state$dates, na.rm = TRUE),
          #   max = max(state$dates + 1, na.rm = TRUE),
          #   value = c(
          #     min(state$dates, na.rm = TRUE),
          #     max(state$dates + 1, na.rm = TRUE)
          #   ),
          # )
        },
        ignoreInit = TRUE
      )

      observeEvent(input$performance_indicators, {
        state$current_indicator <- input$performance_indicators
      })


      observeEvent(input$date_range, {
        state$data_summary_filtered <- state$data_full %>%
          dplyr::filter(
            country == state$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected,
            dplyr::between(transaction_date, input$date_range[1], input$date_range[2])
          ) %>%
          create_data_summary()

        state$dates <- input$date_range
      })

      observeEvent(input$loess_span, {
        state$loess_span <- input$loess_span
      })

      outputOptions(output, "performance_indicators", suspendWhenHidden = FALSE)
    }
  )
}
