#' Show_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
#'
mod_Show_tables_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    box(
        #dataTableOutput(ns("table")),
        title = actionButton(ns("info"), "",icon = icon("circle-info")),
        DTOutput(ns("table")),
        #actionButton(ns("finish"), i18n$t("Valider changement")),
        width = NULL,
        style = "overflow-x: scroll;",
        collapsible = T,
        solidHeader = TRUE
      )#,
    #verbatimTextOutput(ns("test"))
    )
}

#' Show_tables Server Functions
#'
#' @noRd
mod_Show_tables_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    data_forme <- reactive({
      r$data_forme[[as.integer(r$indice)]]
    })

    observeEvent(input$info,{
      message <- as.character(
        i18n$t(
          "Unités tableau : la surface est en km2 _ l'abondance est en nombre/km2 _ la biomasse est en kg/km2 _ chaque espèce est en nombre/km2. Dans ce tableau il est possible de modifier la colonne saison pour mieux s'adapter aux conditions d'échantillonnages."
          )
      )
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    #output$test <- renderPrint({
    #  head(data())
    #})

    #outPut de la table
    output$table <- renderDT({
      # Create a datatable with 'saison' column only editable
      datatable(data_forme(),
                editable = list(target = 'cell',
                                disable = list(columns = c(0,1,2,4,
                                                           5,6,7,8,9,10,
                                                           11,12,13,14))),
                rownames = FALSE)
    })


    observe({
      r$table_modif_season <- data_forme()
      # this new variable will save the change from the user
    })

    # Update the data when a cell is edited
    newData <- reactive({
      if(is.null(input$table_cell_edit)){return(data_forme())}
      info <- input$table_cell_edit


      # Extract the row and column index and the updated value
      row <- info$row
      col <- info$col + 1 # Adjust for 1-based indexing
      value <- info$value

      # Update the data
      newData <- r$table_modif_season
      newData[row, col] <- value

      r$table_modif_season <- newData
      newData
    })

    data <- eventReactive(input$table_cell_edit,{
      newData()
    })

    observe({
      r$data_form_modif <- data()
    })

}

## To be copied in the UI
# mod_Show_tables_ui("Show_tables_1")

## To be copied in the server
# mod_Show_tables_server("Show_tables_1")
