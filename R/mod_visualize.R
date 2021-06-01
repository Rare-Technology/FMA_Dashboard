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
        uiOutput(ns("plot_holder")) %>% withSpinner(color = SPINNER_COLOR)

        

    
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
      output$plot_holder <- renderUI({
        performance_indicators <- state$current_indicator
        data <- state$data_filtered
        loess_span <- state$loess_span
        sel_species <- state$species$selected[1]
        
        result <- switch(performance_indicators,
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
                           ymin = 0
                         ),
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
                           "Total landings (kg/month)",
                           "Total Catch (kg/month)",
                           loess_span
                         ),
        )
        
        state$current_trend <- result$trend
        
        msg <- ""
        p <- result$p
        
        if(is.null(p)){
          p <- NULL
          msg <- "There was not enough data to create a plot"
        } 
        
        if("try-error" %in% class(p)){
          p <- NULL
          msg <- "There was an error creating the plot"
        }
        
        state$current_plot <- p
        output$plot <- renderPlot(
          suppressWarnings(print(p)), # suppress stat_smooth/geom_smooth warnings
          height = PLOT_HEIGHT, 
          width = PLOT_WIDTH
        )
        
      ui_result <- list()
      
      if(!is.null(p)) 
        ui_result[[1]] <- downloadButton(ns("downloadPlot"),class = "download-button", 'Download Plot')
        
      ui_result <- c(
        ui_result,
        list(
          div(class = 'errormsg', msg),
          plotOutput(ns('plot'))
      )
      )
      
      
      ui_result

      })
      
      output$downloadPlot <- downloadHandler(
        filename = function(){paste0("plot_", tolower(gsub(" ", "_", state$current_indicator)), ".png")},
        content = function(file){
          ggsave(file,plot=state$current_plot)
        })


      outputOptions(output, "plot_holder", suspendWhenHidden = FALSE)
    }
  )
}
