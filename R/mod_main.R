#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets dropdown
mainUI <- function(id) {
  ns <- NS(id)
  fillPage(#sidebarLayout(
    dropdown(
      sidebarUI("sidebarUI"),
      width='300px',
      icon = icon('gear'),
      status = 'success',
      style='material-circle'
    ),
    # mainPanel(
    tabsetPanel(
      id = ns("tabs"),
      tabPanel("Start", startUI("startUI")),
      tabPanel("1. Assess data", dataUI("dataUI")),
      #tabPanel("2. Select indicators", selectUI("selectUI")),
      tabPanel("2. Visualize data", visualizeUI("visualizeUI")),
      tabPanel("3. Interpret results", interpretUI("interpretUI"))#,
      #tabPanel("5. Management plan", managementUI("managementUI"))
      )
    # )
  )#)
}

#' main Server Function
#'
#' @noRd
mainServer <- function(id, state) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$tabs, {
        state$current_tab <- input$tabs
      })
      #outputOptions(output, "Visualize", suspendWhenHidden = FALSE)
    }
  )
}
