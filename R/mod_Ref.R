#' Ref UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Ref_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("reference"))
  )
}

#' Ref Server Functions
#'
#' @noRd
mod_Ref_server <- function(input, output, session, r){
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    tutti_catch <- reactive({
      r$tutti_catch
    })

    tutti_operation <- reactive({
      r$tutti_operation
    })

    upload <- reactive({
      r$upload
    })

    # table des stations de référence
    output$reference <- renderUI({
      checkboxGroupInput(
        ns("reference"),
        label = i18n$t("Si besoin séléctionner les stations à retirer :"),
        choices = unique(tutti_operation_bis()[which(is.na(tutti_operation_bis()$impact_date_1)), ]$Code_Station),
        inline = T
      )
    })
    #Les stations à retirer
    station_ban <- reactive({
      input$reference
    })

    #### check point ######
    #output$summary <- renderPrint({
    #  if(is.null(station_ban())){return("No data")}
    #  summary(station_ban())
    #})

    # Zones (fixer le nombre de zone à 1 pour le mode simple)
    zones <- reactive({
      1
    })


    module <- reactive({
      r$module
    })

    #update suite au chargement de la sauvergarde des stations à ban (le reste des udates sont dans le module traitement)
    observeEvent(module()[[1]], {
      # recharge la sauvegarde à chaque modif des stations d'impact car sinon les informations s'effacent
      updateCheckboxGroupInput(inputId = "reference", selected = upload()[[4]][[1]])
    })


    # Représentation des stations : prepare un dataframe pour la carte leaflet
    tutti_operation_bis <-
      reactive({
        if(is.null(tutti_operation())){return()}
        pre_leaflet(tutti_operation(), module()[[1]], module()[[2]], zones())
      })

    # filtrer les stations à retirer
    tutti_operation_filtre <- reactive({
      if(is.null(tutti_operation_bis())){return()}
      if (is.null(station_ban())) {
        tutti_operation_bis()
      } else {
        tutti_operation_bis()[-which(tutti_operation_bis()$Code_Station %in% station_ban()), ]
      }
    })

    tutti_catch_filtre <- reactive({
      if (is.null(station_ban())) {
        tutti_catch()
      } else {
        tutti_catch()[-which(tutti_catch()$Code_Station %in% station_ban()), ]
      }
    })

    observe({
      r$tutti_operation <- tutti_operation()
    })

    observe({
      r$tutti_operation_bis <- tutti_operation_bis()
    })

    observe({
      r$tutti_operation_filtre <- tutti_operation_filtre()
    })

    observe({
      r$tutti_catch_filtre <- tutti_catch_filtre()
    })

    observe({
      r$station_ban <- station_ban()
    })

    observe({
      r$zones <- zones()
    })

}

## To be copied in the UI
# mod_Ref_ui("Ref_1")

## To be copied in the server
# mod_Ref_server("Ref_1")
