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
      
      
      output$plot <- renderPlot({
        plot_fishing_gear(state$data_filtered)
      })
      
      output$plotUI <- renderUI({

        list(
          h3("A plot"),
          plotOutput(ns("plot"))
        )
      })
      
    }
  )
}
