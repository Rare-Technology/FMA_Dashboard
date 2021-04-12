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
        tabPanel(span("Geography"), sidebarGeoUI("sidebarGeoUI")),
        tabPanel(span("Stock", title="Select fisheries family and species"), sidebarStockUI("sidebarStockUI")),
        tabPanel(span("Indicator", title="Fisheries performance indicator"), sidebarIndicatorUI("sideparIndicatorUI"))
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
