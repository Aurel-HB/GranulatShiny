#' Map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("carte"))
  )
}

#' Map Server Functions
#'
#' @noRd
mod_Map_server <- function(input, output, session, r){
    ns <- session$ns

    tutti_operation_bis <- reactive({
      r$tutti_operation_bis
    })

    tutti_operation_filtre <- reactive({
      r$tutti_operation_filtre
    })

    shape <- reactive({
      r$shape
    })

    #leaflet
    output$carte <- renderLeaflet({
      if(is.null(tutti_operation_bis())){return()}
      carte <- leaflet_maker(tutti_operation_filtre(), shape())
    })
}

## To be copied in the UI
# mod_Map_ui("Map_1")

## To be copied in the server
# mod_Map_server("Map_1")
