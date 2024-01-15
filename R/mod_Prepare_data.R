#' Prepare_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Prepare_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("button"))
    #actionButton(ns("go"), "Mettre en forme",icon = icon("dragon"))

  )
}

#' Prepare_data Server Functions
#'
#' @noRd
mod_Prepare_data_server <- function(input, output, session, r){
    ns <- session$ns

    module <- reactive({
      r$module
    })

    station_ban <- reactive({
      r$station_ban
    })

    tutti_operation_filtre <- reactive({
      r$tutti_operation_filtre
    })

    tutti_catch_filtre <- reactive({
      r$tutti_catch_filtre
    })

    zones <- reactive({
      r$zones
    })

    # sauvegarde des inputs
    save <- reactive({
      save <- data.frame(
        stations = as.character(NA),
        dates_deb = as.Date(NA),
        dates_fin = as.Date(NA),
        ban = as.character(NA),
        stringsAsFactors = FALSE
      )
      save$stations <-
        paste(as.character(module()[[1]][[1]]), collapse = "/")
      save$dates_deb <- module()[[2]][[1]][1]
      save$dates_fin <- module()[[2]][[1]][2]
      save$ban <- paste(as.character(station_ban()), collapse = "/")
      save
    })



    # Mettre en forme et calcul des indicateurs
    data_forme <- eventReactive(input$go, {
      if(is.null(tutti_catch_filtre())){return()}
      if(is.null(tutti_operation_filtre())){return()}
      global_function(tutti_catch_filtre(), # fonction qui englobe toutes les autres
                      tutti_operation_filtre(),
                      module()[[1]],
                      module()[[2]],
                      zones())
    }) # data_forme est une liste de 5 tableaux


   output$button <- renderUI({
     if(is.null(tutti_catch_filtre())){return()}
     if(is.null(tutti_operation_filtre())){return()}
     actionButton(ns("go"), "Mettre en forme",icon = icon("dragon"))
   })



    observe({
      r$save <- save()
    })

    observeEvent(input$go, {
      r$button <- TRUE
    })

   observe({
     if(is.null(data_forme())){return()}
      r$data_forme <- data_forme()
     })

}

## To be copied in the UI
# mod_Prepare_data_ui("Prepare_data_1")

## To be copied in the server
# mod_Prepare_data_server("Prepare_data_1")
