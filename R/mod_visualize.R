#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
visualizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plotUI"))
  )
}

#'  Server Function
#'
#' @noRd
visualizeServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # TODO: careful about reactives
      output$plot <- renderPlot({
        pi <- state$performance_indicators
        data <- state$data_filtered
        
       switch (pi,
         "Fishing Gear" = plot_fishing_gear(data),
         "Reporting Effort" = plot_reporting_effort(data, 0.5)
       )
        
      })
      
      output$plotUI <- renderUI({

        list(
          plotOutput(ns("plot"))
        )
      })
      
    }
  )
}
