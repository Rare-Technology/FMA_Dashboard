#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(class='container',
      sidebarLayout(
        sidebarUI("sidebarUI"),
        mainPanel(
          tabsetPanel(
            tabPanel("Data", tableDataUI("tableDataUI")),
            tabPanel("Select", div("2. Select indicators")),
            tabPanel("Visualize", div("3. Visualize data")),
            tabPanel("Interpret", div("4. Interpret results")),
            tabPanel("Plan", div("5. Management plan"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'rarefma'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

