#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # The state object is read in automatically from state.R

  # Sidebar
  sidebarServer("sidebarUI")
  sidebarGeoServer("sidebarGeoUI", state)
  sidebarFamServer("sidebarFamUI", state)
  
  
  # Main
  tableDataServer("tableDataUI", state)

}
