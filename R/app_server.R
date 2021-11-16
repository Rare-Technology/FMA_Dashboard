#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # the next few lines are not so elegant but get the job done: get the value
  # of the cookie from portal.rare.org that determines the language
  http_cookie <- session$request$HTTP_COOKIE
  if (is.null(http_cookie)) { # this case is for running instances locally
    sel_language <- "en_US"
  } else {
    cookies <- strsplit(http_cookie, split = "; ")[[1]]
    language_cookie <- cookies[startsWith(cookies, "trp_language=")] 
    sel_language <- strsplit(language_cookie, "=")[[1]][2]
  }
  
  script <- load_script(sel_language)
  state <- initialize_state()

  # Main application
  mainServer("mainUI", state, script)

  # Sidebar
  sidebarServer("sidebarUI")
  sidebarGeoServer("sidebarGeoUI", state)
  sidebarStockServer("sidebarStockUI", state)
  sidebarIndicatorServer("sidebarIndicatorUI", state)
  sidebarHelpServer("sidebarHelpUI")


  # Main panel
  startServer('startUI')
  dataServer("dataUI", state)
  selectServer("selectUI")
  visualizeServer("visualizeUI", state)
  interpretServer("interpretUI", state)
  managementServer("managementUI")
  #outputOptions(output, "visualizeUI-plot", suspendWhenHidden = FALSE)
}
