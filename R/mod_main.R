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
  fillPage(
      tagList(
        useShinyjs(),
        extendShinyjs(script="www/toggleFullScreen.js", functions=c("toggleFullScreen")),
        div(class='flow-div',
            dropdown(id='step1',
                     sidebarUI("sidebarUI"),
                     width='300px',
                     size='sm',
                     icon = icon('filter'),
                     status = 'success',
                     style='material-circle'
            ),
            div(style='flex-grow: 1;'),
            div(id='help-button', icon('question-circle-o'), onclick='tour()'),
            div(class = 'fs-button',
                HTML(
                  "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' fill='currentColor' class='bi bi-fullscreen' viewBox='0 0 16 16'>
            <path d='M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z'/>
          </svg>"),
                onclick = "shinyjs.toggleFullScreen();"
            )
        ),
        uiOutput(ns("tabPanels"))
      )
    )
  # tagList(
  #   uiOutput(ns("main"))
  # )
}

#' main Server Function
#'
#' @noRd
mainServer <- function(id, state, script) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$tabPanels <- renderUI({
        ui <- list(
          # mainPanel(
          tabsetPanel(id = ns("tabs"),
            tabPanel(script$mod_main$start, startUI("startUI")) %>% tagAppendAttributes(id='startPanel'),
            tabPanel(script$mod_main$tab1, dataUI("dataUI")),
            tabPanel(script$mod_main$tab2, visualizeUI("visualizeUI")),
            tabPanel(script$mod_main$tab3, interpretUI("interpretUI"))#,
            #tabPanel("5. Management plan", managementUI("managementUI"))
          )
        )
        ui
      })
      
      observeEvent(input$tabs, {
        state$current_tab <- input$tabs
      })
      #outputOptions(output, "Visualize", suspendWhenHidden = FALSE)
    }
  )
}
