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
        tabPanel("Family & species", sidebarStockUI("sidebarStockUI")),
        tabPanel("Performance indicators", sidebarIndicatorUI("sidebarIndicatorUI"))
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
