#'  performance indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarHelpUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = 'guidelink', 
        a(
          span(
            icon("download"), 
            p("Fisheries Management Assessment Users Guide")
          ), 
          href = "/www/FMA_guidance.pdf"
          )
        )
  )
}

#' sidebar performance indicators Server Function
#'
#' @noRd
sidebarHelpServer <- function(id, state) {
  ns <- NS(id)
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}
