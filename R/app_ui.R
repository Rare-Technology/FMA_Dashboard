#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    mainUI("mainUI")
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    # bundle_resources(
    #   path = app_sys("app/www"),
    #   app_title = "rarefma"
    # )
    # need this line so that changes to custom.css take effect!
    # this might be a bug on golem's end?
    tags$link(rel='stylesheet', type='text/css', href='www/custom.css'),
    tags$link(rel='stylesheet', type='text/css', href='www/introjs.min.css'),
    tags$script(src='www/intro.min.js'),
    tags$script(src='www/tour.js')
  )
}
