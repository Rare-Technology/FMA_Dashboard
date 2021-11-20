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
    uiOutput(ns("message"))
  )
}
    
#' start Server Functions
#'
#' @noRd 
startServer <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$message <- renderUI({
      ui <- list(
        div(class='startpage',
          tags$h1(tr(state, "Welcome")),
          
          tags$p(tr(state, "To get started, click on"), icon('filter'),
                 tr(state, "to select a country then select the managed access areas you would like to see. For more information, click on"), icon("question-circle-o")),
          div(class = 'timeouttxt',
              h3(class = 'timeouttitle', tr(state, "Please note:")),
              p(tr(state, "This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page."))
          ),
          br(),
          tags$img(id="start_banner", src="www/start_banner.png")
        )
      )
      
      ui
    })
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
