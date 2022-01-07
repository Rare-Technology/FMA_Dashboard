#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
visualizeUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("plot_holder")) %>% withSpinner(color = SPINNER_COLOR)
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
        use_MA_facet <- state$facet
        loess_span <- state$loess_span
        sel_species <- state$species$selected[1]

        if (performance_indicators == tr(state, "Fishing Gear")) {
          result <- plot_fishing_gear(data, state)
        } else if (performance_indicators == tr(state, "Reporting Effort")) {
          result <- plot_reporting_effort(data, loess_span)
        } else if (performance_indicators == tr(state, "Species Composition")) {
          result <- plot_trend_smooth(
            data,
            species,
            count_unique,
            tr(state, "Number of species in the catch"),
            tr(state, "Total number of species recorded in the catch"),
            loess_span,
            ymin = 0,
            use_MA_facet = use_MA_facet
          )
        } else if (performance_indicators == tr(state, "Average Length")) {
          result <- plot_trend_smooth(
            data,
            length,
            mean,
            tr(state, "Average Length"),
            paste(tr(state, "Average length"), "(cm)", sep=" "),
            loess_span,
            ymin = 0,
            use_MA_facet = use_MA_facet
          )
        } else if (performance_indicators == tr(state, "Average Trophic Level")) {
          result <- plot_trend_smooth(
            data,
            trophic_level,
            mean,
            tr(state, "Average trophic level"),
            tr(state, "Average trophic level"),
            loess_span,
            use_MA_facet = use_MA_facet
          )
        } else if (performance_indicators == tr(state, "Size Structure")) {
          result <- plot_size_structure(data, sel_species)
        } else if (performance_indicators == tr(state, "Size Proportions")) {
          result <- plot_size_proportions(data, sel_species)
        } else if (performance_indicators == "CPUE") {
          result <- plot_cpue(data, loess_span, ymin = 0)
        } else if (performance_indicators == tr(state, "Total Landings")) {
          result <- plot_trend_smooth(
            data,
            weight_kg,
            sum,
            tr(state, "Total landings (kg/month)"),
            tr(state, "Total catch (kg/month)"),
            loess_span,
            use_MA_facet = use_MA_facet
          )
        }
        
        state$current_trend <- result$trend
        
        msg <- ""
        p <- result$p
        
        if(is.null(p)){
          p <- NULL
          msg <- "There was not enough data to create a plot"
          if(performance_indicators %in% c("Size Structure", "Size Proportions"))
            msg <- "At least 100 records per species per month are required to create this plot"
        } else if("try-error" %in% class(p)){
            p <- NULL
            msg <- "There was an error creating the plot"
        } else {
            state$current_plot_data <- result$data
            if (performance_indicators != "Reporting Effort") {
              state$current_plot <- p + ggplot2::labs(caption=WATERMARK_LABEL)
            } else {
              # have to treat Reporting Effort plot differently because result$p is a
              # cowplot object and you can't just "+ labs" it
              state$current_plot <- cowplot::plot_grid(result$subplots[[1]] + ggplot2::labs(caption=''),
                result$subplots[[2]] + ggplot2::labs(caption=WATERMARK_LABEL),
                ncol = 2
              )
            }
        }

        output$plot <- renderPlot(
          suppressWarnings(print(p)), # suppress stat_smooth/geom_smooth warnings
          height = PLOT_HEIGHT, 
          width = PLOT_WIDTH
        )
        
      ui_result <- list()
      
      # output$reference_desc <- gt::render_gt({
      #   create_gt_table(
      #     fma_reference_points %>%
      #       dplyr::filter(`Performance Indicator` == performance_indicators),
      #     "Performance Indicator"
      #   )
      # })
      
      if(!is.null(p)) {
        plot_info <- display_filters(state)
        
        ui_result <- c(ui_result,
          list(
            div(class='display-filters',
                HTML(paste(plot_info, collapse=' '))),
            div(class='download-button',
                downloadButton(ns("downloadPlot"), tr(state, 'Download plot'))),
            plotOutput(ns('plot'), width='1000px') %>% 
              # couldn't figure out how to do this in custom.css
              tagAppendAttributes(style="margin: 0 auto;")
          )
        )
      } else {
        ui_result <- c(ui_result, list(div(class = 'errormsg', msg)))
      }    
      
      ui_result

      })
      
      output$downloadPlot <- downloadHandler(
        filename = function(){paste0("fma_", tolower(gsub(" ", "_", state$current_indicator)), ".zip")},
        content = function(file){
          wd <- getwd()
          setwd(tempdir())
          
          meta_name <- 'filters.txt'
          data_name <- 'data.csv'
          plot_name <- paste0("plot_", tolower(gsub(" ", "_", state$current_indicator)), ".png")
          
          filters_text <- display_filters(state, html=FALSE)
          write(filters_text, meta_name)
          write.csv(state$current_plot_data, data_name, row.names = FALSE)
          ggsave(plot_name,plot=state$current_plot, width = 27, height = 20, units = "cm")
          
          fs = c(meta_name, data_name, plot_name)
          zip(zipfile=file, files=fs)
          
          setwd(wd)
        },
        contentType='application/zip')


      outputOptions(output, "plot_holder", suspendWhenHidden = FALSE)
    }
  )
}
