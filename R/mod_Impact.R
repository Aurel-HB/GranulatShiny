#' Impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Impact_ui <- function(id){
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
      #verbatimTextOutput(ns("testobjet")),
      uiOutput(ns("datesOut")),
      uiOutput(ns("stationOut"))
  )
}

#' Impact Server Functions
#'
#' @noRd
mod_Impact_server <- function(input, output, session, r){
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    ########### charge the data in the mod #####
    tutti_operation <- reactive({
      r$tutti_operation
    })

    #### check point ###
    #output$testobjet <- renderPrint({
    #  if(is.null(tutti_operation())){return("No data")}
    #  TRUE
    #})

    upload <- reactive({
      r$upload
    })

    ########## creation of the traitment ####
    output$datesOut <-
      renderUI({
        if(is.null(tutti_operation())){return()}
        dateRangeInput(
          ns("dates"),
          i18n$t("Dates de la période d'activité :"),
          start = min(tutti_operation()$DateDeb),
          end = max(tutti_operation()$DateDeb),
          #start = min(as.Date(
          #  tutti_operation()$DateDeb,  "%d/%m/%Y"
          #)),
          #end = max(as.Date(
          #  tutti_operation()$DateDeb,  "%d/%m/%Y"
          #)),
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "fr",
          separator = " jusqu'au "
        )
      })


    output$stationOut <- renderUI({
      selectInput(
        ns("station"),
        i18n$t("Stations à l'intérieur de la zone d'étude :"),
        multiple = T,
        choices = levels(as.factor(tutti_operation()$Code_Station))
      )
    })

    #### this allow to directly use the upload
    observeEvent(upload(), {
      updateSelectInput(inputId =  "station", selected = upload()[[1]][[1]])
      updateDateRangeInput(inputId = "dates", start = upload()[[2]][[1]],
                           end = upload()[[3]][[1]] )
    })

    ls1 <- reactive({
      list(
        input$station,
        input$dates
      )
    })

    ls_station <-
      reactive({
        list(ls1()[[1]])
      })
    ls_dates <-
      reactive({
        list(ls1()[[2]])
      })

    liste<-reactive({list(
      ls_station(),
      ls_dates()
    )})


    # module correspond to the list of impact station with associate date ?
    observe({
      r$module<-liste()
    })

    observe({
      r$tutti_operation <-tutti_operation()
    })
}

## To be copied in the UI
# mod_Impact_ui("Impact_1")

## To be copied in the server
# mod_Impact_server("Impact_1")
