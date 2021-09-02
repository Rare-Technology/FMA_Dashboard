#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
interpretUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('harvest_controls_holder'))
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
      
      output$harvest_controls_holder <- renderUI({
        indicator <- state$current_indicator
        trend <- state$current_trend
        msg <- NULL
        tbl <- NULL

        if(is.null(trend)) msg <- "A trend could not be estimated"
        
        if(!is.null(trend)){
          tbl <-  fma_harvest_controls %>%
            dplyr::select(-`Assessment Result TRP/LRP`, -`Management Response`) %>%
            dplyr::filter(
              `Performance Indicator` == indicator,
              `Assessment Result` == trend
            ) %>%
            create_gt_table(
              "Performance Indicator"
            )
        }

   
        output$harvest_controls <- gt::render_gt(
          {
            tbl

          },
          height = gt::px(STATIC_TABLE_HEIGHT)
        )
        
        tagList(
          div(class = 'errormsg', msg),
          gt::gt_output(outputId = ns("harvest_controls"))
        )
        
        
      })

    }
  )
}
