#' Choix_loi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Choix_loi_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    box( title = textOutput(ns("variable_y")),
         solidHeader = TRUE,
         collapsible = FALSE,
         width = "100%",
         height = 45#,
         #textOutput(ns("variable_y"))
    ),
    selectInput(
      ns("loi"),
      i18n$t("Sélectionnez la loi de distribution"),
      c(
        "Normale",
        "Binomiale",
        "Poisson",
        "Binomiale négative",
        "Gamma log",
        "Lognormale"
      )),
    box(title = i18n$t("Legende"),
        collapsible = FALSE,
        solidHeader = TRUE,
        status = "info",
        width = "100%",
        #height = 100,
        uiOutput(ns("legend_density")),
        uiOutput(ns("legend_law"))
      ),
    box( solidHeader = FALSE,
         collapsible = FALSE,
         width = "100%",
         #height = 55,
         textOutput(ns("nb_value"))
         ),
    hr(),
    actionButton("go_modelo", i18n$t("Statistiques inférentielles"), icon = icon("ship"))
  )
}

#' Choix_loi Server Functions
#'
#' @noRd
mod_Choix_loi_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- session$ns

  data_analyse <- reactive({
    r$data_analyse
  })


  # Render UI parametrage glmms

  output$variable_y <- renderText({
    paste(i18n$t("La variable est"), r$var_name, sep = " ")
  })

  loi <- reactive({
    input$loi
  })

  observe({
    r$loi <- loi()
  })

  output$legend_density <- renderUI({
    # Use tags$span() to generate a <span> with the specified color
    tags$span(
      style = paste0("color:#0066FF","; font-size:medium"),
      textOutput(ns("density_text"))
    )
  })

  output$density_text <- renderText({return(
    i18n$t("La courbe bleue correspond à la fonction de densité de la variable")
  )
  })

  output$legend_law <- renderUI({
    # Use tags$span() to generate a <span> with the specified color
    tags$span(
      style = paste0("color:#00CC99","; font-size:medium"),
      textOutput(ns("law_text"))
    )
  })

  output$law_text <- renderText({
    return(
      i18n$t("La courbe verte correspond à la loi de distribution tel que les paramètres sont estimés à partir de la moyenne et de l'écart-type de la variable")
      )
  })

  data_analyse <- reactive({
    r$data_analyse
  })

  summa <- reactive({
    if(is.null(data_analyse())){return()}
      numeric_summary(as.numeric(data_analyse()[,1]), names(data_analyse())[1])
  })

  nb_tot <- reactive({
    if(is.null(data_analyse())){return()}
    length(as.numeric(data_analyse()[,1]))
  })


  nb_value <- reactive({
    if(is.null(data_analyse())){return()}
      nb_tot() - summa()[2]
      })

  output$nb_value <- renderText({
    if (is.null(nb_value())){return()}
    if(nb_value() >= 30){
      return(i18n$t("Après avoir choisi une distribution vous pouvez passer à la construction du modèle"))
    }
    if(nb_value() < 30){
      return("Vous n'avez pas assez de données pour réaliser un GLM ou un GLMM")
    }
  })

}

## To be copied in the UI
# mod_Choix_loi_ui("Choix_loi_1")

## To be copied in the server
# mod_Choix_loi_server("Choix_loi_1")
