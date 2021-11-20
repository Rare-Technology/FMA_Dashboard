#' main_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "table-holder",
      DT::DTOutput(ns("main_data"))
    )
  )
}

#' main_data Server Function
#'
#' @noRd
dataServer <- function(id, state) {
  moduleServer(
    id,
    function(input, output, server) {
      observeEvent(state$data_summary_filtered, {
        Country <- tr(state, "Country")
        Subnational <- tr(state, "Subnational")
        Local <- tr(state, "Local")
        MA <- tr(state, "Managed access area")
        Family <- tr(state, "Family")
        Species <- tr(state, "Species")
        table_data <- state$data_summary_filtered %>%
          dplyr::rename(
            !!Country := country,
            !!Subnational := subnational,
            !!Local := local,
            !!MA := maa,
            !!Family := family,
            !!Species := species
          )


        tbl <- DT::datatable(
          data = table_data,
          class = "display nowrap",
          rownames = FALSE,
          # container = tags$table(
          #   class = "table table-striped table-hover table-header-vertalign",
          #   DT::tableHeader(table_data)
          # ),
          options = list(
            columnDefs = table_column_alignment(table_data),
            pageLength = 100,
            dom = "frtp",
            scrollX = TRUE,
            scrollY = "600px",
            processing = FALSE,
            scrollCollapse = TRUE,
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            )
          )
        )


        output$main_data <- DT::renderDT(tbl)
      })
    }
  )
}

## To be copied in the UI
# mod_main_data_ui("main_data_ui_1")

## To be copied in the server
# callModule(mod_main_data_server, "main_data_ui_1")
