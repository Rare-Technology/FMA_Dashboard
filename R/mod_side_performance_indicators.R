#'  performance indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyjs reset runjs
sidebarIndicatorUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("performance_indicators"))
  )
}

#' sidebar performance indicators Server Function
#'
#' @noRd
sidebarIndicatorServer <- function(id, state) {
  ns <- NS(id)


  moduleServer(
    id,
    function(input, output, session) {

      # Dates slider

      output$performance_indicators <- renderUI({
        current_tab <- state$current_tab
        current_date_range <- state$current_date_range
        current_indicator <- state$current_indicator
        current_min_records <- state$current_min_records
        
        ui <- list()
  
        ui[['date_range_title']] <- div(class='sidetitle', tr(state, 'Time Period'))
        ui[["date_range"]] <- div(
          dateRangeInput(ns("date_range"),
                         label = tr(state, "Select date range"),
                         start = current_date_range$valmin,
                         end = current_date_range$valmax,
                         min = current_date_range$min,
                         max = current_date_range$max,
                         startview = 'year',
          )
        )
        # ---- Select, Visualize, Interpret tabs
        # show indicators only on plot and interpret tabs
        if (current_tab %in% c(tr(state, "2. Visualize data"), tr(state, "3. Interpret results"))) {
          ui[["performance_indicators_title"]] <- div(class='sidetitle', tr(state, "Indicator"))
          ui[["performance_indicators"]] <- 
            div(class = " pi_widget", 
            selectInput(
              inputId = ns("performance_indicators"),
              label = div(
                tr(state, "Select indicator"), 
                actionLink(inputId = ns("show_defs"), label = "(definitions)")
                ),
              choices = c(
                tr(state, "Fishing Gear"),
                tr(state, "Reporting Effort"),
                tr(state, "Species Composition"),
                # "Fished:Unfished Ratio",
                tr(state, "Average Length"),
                tr(state, "Average Trophic Level"),
                # "Spawning Potential Ratio",
                tr(state, "Size Structure"),
                tr(state,  "Size Proportions"),
                tr(state,  "CPUE"),
                tr(state, "Total Landings")
              ),
              selected = current_indicator
            )
            )
         
          
        }
        
        ### Plotting options
        if (current_tab %in% c(tr(state, "2. Visualize data"))) {
          ui[["plot_options_title"]] <- div(class = "sidetitle", tr(state, "Plot options"))
          if (current_indicator %in% c(
            tr(state, 'Species Composition'),
            tr(state, 'Average Length'),
            tr(state, 'CPUE'),
            tr(state, 'Total Landings'),
            tr(state, 'Average Trophic Level')
          )) {
            ui[["ma_facet"]] <- tagList(
              br(),
              div(id = "ma-facet",
                materialSwitch(
                  ns("ma_facet"),
                  tr(state, "Group by MA"),
                  value = FALSE,
                  width = "100%",
                  status = "primary"
                )
              )
            )
          }
          # ui[["family_facet"]] <- tagList(
          #   br(),
          #   div(id = "family-facet",
          #       materialSwitch(
          #         ns("family_facet"),
          #         tr(state, "Group by family"),
          #         value = FALSE,
          #         width = "100%",
          #         status = "primary"
          #       )
          #   )
          # )
          if (current_indicator %in% c(tr(state, "Size Structure"), tr(state, "Size Proportions"))) {
            ui[["species_facet"]] <- tagList(
              br(),
              div(id = "species-facet",
                  materialSwitch(
                    ns("species_facet"),
                    tr(state, "Group by species"),
                    value = FALSE,
                    width = "100%",
                    status = "primary"
                  )
              )
            )
          }

          # Currently these two inputs are NULL despite having (or should have...) a default value
          ui[["y-scale"]] <- tagList(
            br(),
            materialSwitch(
              ns("fixed_yscale"),
              tr(state, "Fixed Y-axis"),
              value = TRUE,
              width = "100%",
              status = "primary"
            )
          )

          # show loess slider only when plotting curves
          if (!(current_indicator %in% c(tr(state, "Fishing Gear"), tr(state, "Size Structure"), tr(state, "Size Proportions")))) {
            # ui[['other_title']] <- div(class='sidetitle', tr(state, 'Other'))
            ui[["loess_span"]] <- div(
              class = "smooth_slider pi_widget",
              sliderInput(
                inputId = ns("loess_span"),
                label = tooltip_label("tip-smooth", tr(state, "Change curve smoothing")),
                min = 0.1, max = 1,
                value = 0.5,
                step = 0.1,
                ticks = FALSE
              )
            )
          }
          ui[['tippy']] <- list(
            tippy_class_alt("tip-smooth", "Controls the amount of smoothing of the loess curve. Smaller numbers produce wigglier lines, larger numbers produce smoother lines"),
            tippy_class_alt("tip-min-species", "We recommend selecting at least 100 records per species per month for robust analysis")
          )
        }
        ui
      })

      observeEvent(input$performance_indicators, {
        state$current_indicator <- input$performance_indicators
      }, ignoreInit = TRUE)
      
      observeEvent(input$date_range, {
        
        data_filtered <- state$data_full %>%
          dplyr::filter(
            country == state$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected,
            dplyr::between(transaction_date, input$date_range[1], input$date_range[2])
          )
        
        state$data_filtered <- data_filtered
   
        state$data_summary_filtered <- data_filtered %>%
          create_data_summary()

        state$current_date_range$valmin <- input$date_range[1]
        state$current_date_range$valmax <- input$date_range[2]
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$ma_facet, {
        state$ma_facet <- input$ma_facet
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input$family_facet, {
        state$family_facet <- input$family_facet
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input$species_facet, {
        state$species_facet <- input$species_facet
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input$fixed_yscale, {
        state$fixed_yscale <- input$fixed_yscale
      })

      observeEvent(input$loess_span, {
        state$loess_span <- input$loess_span
      })
      
      observeEvent(input$show_defs, {
        showModal(modalDialog(
          title = "Performance indicator definitions",
          gt::gt_output(outputId = ns("reference_points")),
          easyClose = TRUE,
          footer = modalButton("Dismiss"),
          #footer = NULL,
          size = "l"
        ))
      })
      
      observeEvent(state$resetFilters, {
        reset("performance_indicators")
      }, ignoreInit = TRUE)
      
      output$reference_points <-
        gt::render_gt(
          {
            create_gt_table(
              fma_reference_points,
              "Performance Indicator"
            )
          },
          height = gt::px(STATIC_TABLE_HEIGHT)
        )
      
      # Commented out per git 21
      # observeEvent(input$min_records, {
      #   
      #   
      #   state$current_min_records$value <- input$min_records
      # })

      outputOptions(output, "performance_indicators", suspendWhenHidden = FALSE)
    }
  )
}
