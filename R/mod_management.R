#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
managementUI <- function(id) {
  ns <- NS(id)
  tagList()
}

#'  Server Function
#'
#' @noRd
managementServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {


    }
  )
}
