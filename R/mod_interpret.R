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
    gt::gt_output(outputId = ns("harvest_controls"))
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
      output$harvest_controls <- gt::render_gt(
        {
          indicator <- state$current_indicator
          trend <- state$current_trend
          fma_harvest_controls %>%
            dplyr::select(-`Assessment Result TRP/LRP`, -`Management Response`) %>%
            dplyr::filter(
              `Performance Indicator` == indicator,
              `Assessment Result` == trend
            ) %>%
            create_gt_table(
              "Harvest Controls",
              "(a subtitle)",
              "Performance Indicator"
            )
        },
        height = gt::px(STATIC_TABLE_HEIGHT)
      )
    }
  )
}
