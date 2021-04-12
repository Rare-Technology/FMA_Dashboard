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
    uiOutput(ns('performance_indicators'))
  )
}

#' sidebar performance indicators Server Function
#'
#' @noRd
sidebarIndicatorServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session){

      # Dates slider
      
      output$performance_indicators <- renderUI({
        tab <- state$current_tab
        ui <- tab
        
        # ---- Data tab
        if(tab == "Data"){
          ui <- div(class = 'date_slider',
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
        }
        
        # ---- Select, Visualize, Interpret tabs
        if(tab %in% c("Select", "Visualize", "Interpret")){
          ui <-       selectInput(
            inputId = ns('performance_indicators'),
            label = "Select performance indicators",
            choices = c('Fishing Gear',
                        'Reporting Effort',
                        'Species Composition',
                        'Fished:Unfished Ratio',
                        'Average Length',
                        'Average Trophic Level',
                        #'Spawning Potential Ratio',
                        'Size Structure',
                        'Size Proportions',
                        'CPUE',
                        'Total Landings'),
            selected = state$performance_indicators
          )
        }
        
        ui
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
      
      observeEvent(input$performance_indicators, {
        print(input$performance_indicators)
        state$performance_indicators <- input$performance_indicators
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
        
      })
      
    }
  )
  
  
}

