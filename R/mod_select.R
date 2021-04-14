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
    
      ref_points <- fma_reference_points %>% 
        gt::gt() %>% 
        gt::tab_header(
          title = "Performance Indicators",
          subtitle = "(pick one in the selector on the left)"
        ) %>% 
        gt::tab_style(
          style = list(
            # cell_fill(color = "lightcyan"),
            "font-variant: small-caps;"
          ),
          locations = gt::cells_body(columns = gt::vars(`Performance Indicator`))
        ) %>% 
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#f7e7c3")
            ),
          locations = gt::cells_body(
            rows = seq(1, nrow(fma_reference_points), by = 2)
          )
        ) %>% 
        gt::cols_width(
          gt::contains("Performance") ~ px(125),
          gt::everything() ~ px(145)
        ) %>% 
        gt::tab_options(
          table.font.size = 13
        )

      
      output$reference_points <-
        gt::render_gt(
          expr = ref_points,
          height = gt::px(800),
        )
      
    }
  )
}
