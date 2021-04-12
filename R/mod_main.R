#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mainUI <- function(id){
  ns <- NS(id)
    fluidPage(class='container',
              sidebarLayout(
                sidebarUI("sidebarUI"),
                mainPanel(
                  tabsetPanel(id = ns("tabs"),
                    tabPanel("Data", tableDataUI("tableDataUI")),
                    tabPanel("Select", div("2. Select indicators")),
                    tabPanel("Visualize", div("3. Visualize data")),
                    tabPanel("Interpret", div("4. Interpret results")),
                    tabPanel("Plan", div("5. Management plan"))
                  )
                )
              )
    
  )
}

#' main Server Function
#'
#' @noRd 
mainServer <- function(id, state){
  moduleServer(
    id,
    function(input, output, session){

      observeEvent(input$tabs, {
        state$current_tab <- input$tabs
      })
    }
  )
  
  
}
