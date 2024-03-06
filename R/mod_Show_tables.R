#' Show_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Show_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
        dataTableOutput(ns("contents")),
        width = NULL,
        style = "overflow-x: scroll;",
        collapsible = T,
        solidHeader = TRUE
      )
    )
}

#' Show_tables Server Functions
#'
#' @noRd
mod_Show_tables_server <- function(input, output, session, r){
    ns <- session$ns

    data_forme <- reactive({
      r$data_forme
    })

    #outPut de la table
    output$contents <- renderDataTable({
      if(is.null(data_forme())){return()}
      data_forme()[[as.integer(r$indice)]]

    }, options = list("pageLength" = 10))
}

## To be copied in the UI
# mod_Show_tables_ui("Show_tables_1")

## To be copied in the server
# mod_Show_tables_server("Show_tables_1")
