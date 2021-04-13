#' side UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(class="sidebar",
      tabsetPanel(
        tabPanel("Geography", sidebarGeoUI("sidebarGeoUI")),
        tabPanel("Stock", sidebarStockUI("sidebarStockUI")),
        tabPanel("Indicator", sidebarIndicatorUI("sidebarIndicatorUI"))
      )
  )
  )
}

#' side Server Function
#'
#' @noRd
sidebarServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){

    }
  )


}
