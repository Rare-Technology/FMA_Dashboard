#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
startUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$h1("Welcome!"),
    tags$h2("August 31 2021 Notes"),
    tags$li("Data is finally up to date! Automated data updates coming soon."),
    tags$li("Gear type, reporting efforts, and CPUE plots temporarily disabled due to conflicts with new data."),
    tags$li("Date selection now uses a pop-up calendar instead of a slider."),
    tags$li("MA selections are empty by default.")
  )
}
    
#' start Server Functions
#'
#' @noRd 
startServer <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
