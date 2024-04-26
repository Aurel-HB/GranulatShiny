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
  )
}

#' Prepare_data Server Functions
#'
#' @noRd
mod_Prepare_data_server <- function(input, output, session, r){
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
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

    check_concession <- reactive({
      r$check_concession
    })

    # sauvegarde des inputs
    save <- reactive({
      save <- data.frame(
        stations = as.character(NA),
        dates_deb = as.Date(NA),
        dates_fin = as.Date(NA),
        ban = as.character(NA),
        trawl_opening = as.numeric(NA),
        stringsAsFactors = FALSE
      )
      save$stations <-
        paste(as.character(module()[[1]][[1]]), collapse = "/")
      save$dates_deb <- module()[[2]][[1]][1]
      save$dates_fin <- module()[[2]][[1]][2]
      save$ban <- paste(as.character(station_ban()), collapse = "/")
      save$trawl_opening <- r$trawl_opening
      save
    })


    # save the different species present and convert the name in the same format
    species <- reactive({
      if(is.null(tutti_catch_filtre())){return()}
      species <- unique(tutti_catch_filtre()$Nom_Scientifique)
      species <- str_replace(species, " ", ".")
      species <- str_replace(species, "_", ".")
      species <- str_replace(species, "-", ".")
      return(species)
    })

    observe({
      r$species <- species()
    })

    # Mettre en forme et calcul des indicateurs
    data_forme <- eventReactive(input$go, {
      if(is.null(tutti_catch_filtre())){return()}
      if(is.null(tutti_operation_filtre())){return()}
      global_function(tutti_catch_filtre(), # fonction qui englobe toutes les autres
                      tutti_operation_filtre(),
                      module()[[1]],
                      module()[[2]],
                      zones(),
                      r$trawl_opening)
    }) # data_forme est une liste de 5 tableaux


   output$button <- renderUI({
     if(is.null(tutti_catch_filtre())){return()}
     if(is.null(tutti_operation_filtre())){return()}
     if(check_concession()==FALSE){return()}
     if(r$check_duplica==FALSE){return()}
     if(r$check_date==FALSE){return()}
     if(is.null(r$trawl_opening)){return()}
     actionButton(ns("go"), i18n$t("Mettre en forme"),
                  icon = icon("dragon", style='color: #22A433'))
   })

   # icon("dragon", style='color: #AF1111')
   # icon("dragon", style='color: #22A433')

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
