#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets dropdown
#' @importFrom shinyjs useShinyjs extendShinyjs
mainUI <- function(id) {
  ns <- NS(id)
  div(id = 'fma-body',
  fillPage(
      tagList(
        useShinyjs(),
        extendShinyjs(script="www/toggleFullScreen.js", functions=c("toggleFullScreen")),
        div(class='flow-div',
            dropdown(id='step1',
                     sidebarUI("sidebarUI"),
                     width='400px',
                     size='sm',
                     icon = icon('filter'),
                     status = 'success',
                     style='material-circle'
            ),
            uiOutput(ns("applyFiltersUI")),
            uiOutput(ns("resetFiltersUI")),
            div(style='flex-grow: 1;'),
            div(id='help-button', icon('question-circle-o'), onclick='tour()'),
            div(id="lang-select", selectInput(ns("language"), "", width = 80,
                                              c("EN" = "English",
                                                "ID" = "Bahasa Indonesia",
                                                "BR" = "Portugues (BR)",
                                                "MZ" = "Portugues (MOZ)",
                                                "ES" = "Español",
                                                "PH" = "Philippines"
                                              ))
            ),
            div(class = 'fs-button',
                HTML(
                  "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' fill='currentColor' class='bi bi-fullscreen' viewBox='0 0 16 16'>
                    <path d='M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z'/></svg>"
                ),
                onclick = "shinyjs.toggleFullScreen();"
            )
        ), # flow-div
        uiOutput(ns("tabPanels"))
      ) # tagList
  )) # fillPage
}

#' main Server Function
#'
#' @noRd
mainServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$applyFiltersUI <- renderUI({
        div(id = "apply-filters", actionButton(ns("applyFilters"), tr(state, "Apply filters")))
      })
      
      output$resetFiltersUI <- renderUI({
        div(id="reset-filters", actionButton(ns('resetFilters'), tr(state, "Reset filters")))
      })
      
      output$tabPanels <- renderUI({
        ui <- list(
          # mainPanel(
          tabsetPanel(id = ns("tabs"),
            tabPanel(tr(state, "Start"), startUI("startUI")) %>% tagAppendAttributes(id='startPanel'),
            tabPanel(tr(state, "1. Assess data"), dataUI("dataUI")),
            tabPanel(tr(state, "2. Visualize data"), visualizeUI("visualizeUI")),
            tabPanel(tr(state, "3. Interpret results"), interpretUI("interpretUI"))#,
            #tabPanel("5. Management plan", managementUI("managementUI"))
          )
        )
        ui
      })
      
      observeEvent(input$tabs, {
        state$current_tab <- input$tabs
      })
      
      observeEvent(input$language, {
        state$language <- input$language
      })
      
      observeEvent(input$applyFilters, {
        data_filtered <- state$data_full %>%
          dplyr::filter(
            country %in% stete$country$selected,
            subnational %in% state$subnational$selected,
            local %in% state$local$selected,
            maa %in% state$maa$selected,
            family %in% state$family$selected,
            species %in% state$species$selected,
            
          )
        
        state$data_filtered <- data_filtered
        state$data_summary_filtered <- data_filtered %>%
          create_data_summary()      })
      
      observeEvent(input$resetFilters, {
        state$resetFilters <- input$resetFilters + 1
      })
      #outputOptions(output, "Visualize", suspendWhenHidden = FALSE)
    }
  )
}
