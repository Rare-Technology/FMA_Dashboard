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
  div(class='startpage',
    tags$h1("Welcome!"),
    
    tags$p("To get started, click on ", icon('filter'),
           " to select a country and then select the managed access areas you ",
           " would like to see."),
    div(class = 'timeouttxt',
        h3(class = 'timeouttitle', "Please note"),
        p("This app may time-out if left idle too long, which will cause the",
          " screen to grey-out. To use the app again, refresh the page.")
    ),
    
    tags$h2("September 7 Notes"),
    tags$li("Selected filters are shown above the plot."),
    
    tags$h2("September 2 Notes"),
    tags$li("Gear type, reporting efforts, and CPUE plots are enabled again."),
    tags$li("The sidebar can be toggled on and off. Click on ", icon('filter'), "!"),
    tags$li("Added full screen button. Click the icon on the top right to toggle ",
            " full screen on and off."),
    
    tags$h2("September 1 Notes"),
    tags$li("Updated geological selection menu. Select or deselect all regions ",
            " with one click!"),
    
    tags$h2("August 31 2021 Notes"),
    tags$li("Data is finally up to date! Automated data updates coming soon."),
    tags$li("Gear type, reporting efforts, and CPUE plots temporarily disabled ",
            " due to conflicts with new data."),
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
