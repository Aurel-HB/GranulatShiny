#' Tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tables_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    selectInput(
      ns("list"),
      i18n$t("Quelle table afficher ?"),
      c(
        "Table complete" = "1",
        "Abondance" = "2",
        "Biomasse" = "3",
        "Richesse" = "4",
        "Simpson" = "5"
      ),
    selected = "Table complete"
    ),
    downloadButton(ns("downloadData"), label = i18n$t("Telecharger la table (.csv)")),
    hr(),
    downloadButton(ns("downloadSave"), label = i18n$t("Telecharger les informations rentrées (.csv)")
                   #style='width: 100%; overflow-x: scroll'),
                   ),
    hr(),
    uiOutput(ns("variable")),
    actionButton("gostat1", i18n$t("Statistiques exploratoires"), icon = icon("ship")),
    actionButton("gostat2", i18n$t("Statistiques descriptives"), icon = icon("ship"))
    )
}

#' Tables Server Functions
#'
#' @noRd
mod_Tables_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    save <- reactive({
      r$save
    })

    data_forme <- reactive({
      r$data_forme
    })

    indice <- reactive({
      if(is.null(input$list)){return()}
      input$list
    })

    # Telecharger la sauvegarde
    output$downloadSave <- downloadHandler(
      filename = function() {
        paste("sauvegarde", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(save(),  file)
      }
    )

    variables <- reactive({
      n <- names(as.data.frame(data_forme()[[1]]))
      var_expl <- c("year","station","date","saison","campagne","tow","hauled_surf",
               "traitement","interaction","time") # filter the explanatory variable
      n <- n[!(n %in% var_expl)] # keep only the explicated value
      n
      })

    output$variable <- renderUI({
      if(is.null(data_forme())){return()}
      selectInput(
        ns("var"),
        i18n$t("Choisir un variable pour les statistiques descriptives"),
        choices = levels(as.factor(variables())),
        selected = "Abun"
      )
    })

    data_analyse <- reactive({
      if(is.null(data_forme())){return()}
      v <- data_forme()[[1]][input$var]
      y <- data_forme()[[1]]["year"]
      l <- data_forme()[[1]]["station"]
      s <- data_forme()[[1]]["saison"]
      t <- data_forme()[[1]]["traitement"]
      #i <- data_forme()[[1]]["interaction"]
      c <- data_forme()[[1]]["campagne"]
      data <- data.frame(v, y, l, s, t, c) #i,c)
      names(data) <- c(input$var, "year", "station", "saison", "traitement", "campagne") #"interaction", "campagne")
      data
    })

    observe({
      r$var_name <- input$var
    })

    #Telecharger les données
    nom_data <-
      c("Table complete",
        "Abondance",
        "Biomasse",
        "Richesse",
        "Simpson")

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", nom_data[as.integer(input$list)], ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_forme()[[as.integer(input$list)]],  file)
      }
    )

    observe({
      r$data_analyse <- data_analyse()
    })

    observe({
      r$indice <- indice()
    })


}

## To be copied in the UI
# mod_Tables_ui("Tables_1")

## To be copied in the server
# mod_Tables_server("Tables_1")
