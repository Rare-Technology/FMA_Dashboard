#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    # DT::DTOutput(ns('reference_points'))
    gt::gt_output(outputId = ns("reference_points"))
  )
}

#'  Server Function
#'
#' @noRd
selectServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      # output$reference_points <-
      #   DT::renderDT(
      #     DT::datatable(
      #       reference_points,
      #       rownames = FALSE,
      #       options = list (lengthChange = TRUE,
      #                       pageLength = 15,
      #                       scrollX = TRUE,
      #                       scrollY = "500px")) %>%
      #       DT::formatStyle(columns = 1,
      #                   fontWeight = 'bold')
      #
      #   )





      output$reference_points <-
        gt::render_gt(
          {
            create_gt_table(
              fma_reference_points,
              "Performance Indicators",
              "(pick one in the selector on the left)",
              "Performance Indicator"
            )
          },
          height = gt::px(STATIC_TABLE_HEIGHT)
        )
    }
  )
}
