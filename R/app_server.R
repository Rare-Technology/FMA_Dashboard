#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # The state object is read in automatically from state.R

  # Main application
  mainServer("mainUI", state)
  
  # Sidebar
  sidebarServer("sidebarUI")
  sidebarGeoServer("sidebarGeoUI", state)
  sidebarStockServer("sidebarStockUI", state)
  sidebarIndicatorServer("sidebarIndicatorUI", state)
  
  
  # Main panel
  dataServer("dataUI", state)
  selectServer("selectUI")
  visualizeServer("visualizeUI", state)
  interpretServer("interpretUI")
  managementServer("managementUI")

}
