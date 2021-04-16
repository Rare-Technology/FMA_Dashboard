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
        current_tab <- state$current_tab
        current_date_range <- state$current_date_range
        current_indicator <- state$current_indicator
        current_min_records <- state$current_min_records
        
        ui <- list()

  
     
        # ---- Select, Visualize, Interpret tabs
        if (current_tab %in% c("Select", "Visualize", "Interpret")) {
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
            selected = current_indicator
          ))
        }

        # ---- Data tab
        if (current_tab %in% c("Data", "Visualize")) {
          ui[["date_slider"]] <- div(
            class = "date_slider pi_widget",
            sliderInput(ns("date_range"),
              label = "Select date range",
              min = current_date_range$min,
              max = current_date_range$max + 1,
              value = c(current_date_range$valmin, current_date_range$valmax),
              ticks = FALSE,
              timeFormat = "%F"
            )
          )
        }



        if (current_tab %in% c("Visualize") && !current_indicator %in% c("Fishing Gear", "Size Structure", "Size Proportions")) {
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
        
        if (current_tab %in% c("Visualize") && current_indicator %in% c("Size Structure", "Size Proportions")) {
          ui[["min_records"]] <- div(
            class = "smooth_slider pi_widget",
            sliderInput(
              inputId = ns("min_records"),
              label = "Select min records per species",
              min = current_min_records$min,  
              max = current_min_records$max,
              value = current_min_records$value, 
              step = 10,
              ticks = FALSE)
          )
        }
        
      

        ui
      })



      observeEvent(input$performance_indicators, {
        state$current_indicator <- input$performance_indicators
      })


      observeEvent(input$date_range, {
        
        data_filtered <- state$data_full %>%
          dplyr::filter(
            country == state$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected,
            dplyr::between(transaction_date, input$date_range[1], input$date_range[2])
          )
        
        state$data_filtered <- data_filtered
        
        state$data_summary_filtered <- data_filtered %>%
          create_data_summary()

        state$current_date_range$valmin <- input$date_range[1]
        state$current_date_range$valmax <- input$date_range[2]
        
      }, ignoreInit = TRUE)

      observeEvent(input$loess_span, {
        state$loess_span <- input$loess_span
      })
      
      observeEvent(input$min_records, {
        
        
        state$current_min_records$value <- input$min_records
      })

      outputOptions(output, "performance_indicators", suspendWhenHidden = FALSE)
    }
  )
}
