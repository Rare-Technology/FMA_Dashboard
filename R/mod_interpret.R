#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
interpretUI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

#'  Server Function
#'
#' @noRd
interpretServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}
