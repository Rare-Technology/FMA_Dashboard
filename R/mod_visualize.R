#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
visualizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")) %>% withSpinner(color = SPINNER_COLOR)
  )
}

#'  Server Function
#'
#' @noRd
visualizeServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # TODO: careful about reactives
      output$plot <- renderPlot({
        
        performance_indicators <- state$performance_indicators
        data <- state$data_filtered
        loess_span <- state$loess_span
        sel_species <- state$species$selected[1]
        
       switch (performance_indicators,
         "Fishing Gear" = plot_fishing_gear(data),
         "Reporting Effort" = plot_reporting_effort(data, loess_span),
         "Species Composition" = plot_trend_smooth(
           data, 
           species, 
           count_unique,
           "Number of species in the catch",
           "Total number of species recorded in the catch",
           loess_span,
           ymin = 0
           ),
         "Average Length" = plot_trend_smooth(
           data, 
           length,
           mean,
           "Average length",
           "Average Length (cm)",
           loess_span,
           ymin = 0),
         "Average Trophic Level" = plot_trend_smooth(
           data,
           trophic_level,
           mean,
           "Average trophic level",
           "Average trophic level",
           loess_span
         ),
         "Size Structure" = plot_size_structure(data, sel_species),
         "Size Proportions" = plot_size_proportions(data, sel_species),
         "CPUE" = plot_cpue(data, loess_span, ymin = 0),
         "Total Landings" = plot_trend_smooth(
           data,
           weight_kg,
           sum,
           "Average trophic level",
           "Total landings (kg/month)",
           loess_span
         ),
       )
        
      }, height = PLOT_HEIGHT)
      
      # output$plotUI <- renderUI({
      # 
      #   list(
      #     plotOutput(ns("plot"))
      #   )
      # })
      
    }
  )
}
