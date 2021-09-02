#' side UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
      sidebarGeoUI("sidebarGeoUI"),
      sidebarStockUI("sidebarStockUI"),
      sidebarIndicatorUI("sidebarIndicatorUI"),
      sidebarHelpUI("sidebarHelpUi")
  )
}

#' side Server Function
#'
#' @noRd
sidebarServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
    }
  )
}
