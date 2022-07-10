#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  state <- initialize_state()
  # Main application
  mainServer("mainUI", state)

  # Sidebar
  sidebarServer("sidebarUI")
  sidebarGeoServer("sidebarGeoUI", state)
  sidebarStockServer("sidebarStockUI", state)
  sidebarIndicatorServer("sidebarIndicatorUI", state)
  sidebarHelpServer("sidebarHelpUI")


  # Main panel
  startServer('startUI', state)
  dataServer("dataUI", state)
  selectServer("selectUI")
  visualizeServer("visualizeUI", state)
  interpretServer("interpretUI", state)
  managementServer("managementUI")
  #outputOptions(output, "visualizeUI-plot", suspendWhenHidden = FALSE)
}
