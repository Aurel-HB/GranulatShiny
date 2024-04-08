#' Prepare_modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Prepare_modelling_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    #Input des glmms
    actionButton(ns("info"), "",icon = icon("circle-info")),
      uiOutput(ns("variable_y")),
      selectInput(
        ns("methode"),
        i18n$t("Sélectionnez la méthode statistique"),
        c(
          "GLMM" = "1",
          "GLM" = "2",
          "Permanova" = "3"
        )
      ),
      uiOutput(ns("distribution")),
      uiOutput(ns("covariable")),
      checkboxInput(ns("interaction"), i18n$t("Voulez-vous retirer l'interaction ?")),
      textOutput(ns("info")),

      hr(),
      h4(strong(i18n$t("Formulation du modèle :"))),
      htmlOutput(ns("ecriture_modele")),
      #htmlOutput(ns("ecriture_distribution")),
      #verbatimTextOutput(ns("test")),
      hr(),
      actionButton(ns("go2"), i18n$t("Lancer la modélisation"),
                   icon = icon("dragon", style='color: #22A433')),
      hr(),
      uiOutput(ns("choix_modele")),
      hr(),
      uiOutput(ns("go_represent"))







  )
}

#' Prepare_modelling Server Functions
#'
#' @noRd
mod_Prepare_modelling_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns
    # GLMMs -------------------------------------------------------------------
    # Onglet Creation

    observeEvent(input$info,{
      message <- as.character(list_translate[r$lang][10,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    # Render UI parametrage glmms

    output$variable_y <-
      renderUI({
        selectInput(ns("y"), i18n$t("La variable Y choisie est"), choices = r$var_name)
      })

    output$distribution <- renderUI({
      if(input$methode == 3){return()}
      selectInput(
        ns("distribution"),
        i18n$t("Sélectionnez la loi de distribution"),
        c(
          "Normale",
          "Binomiale" = "binomial",
          "Poisson" = "poisson",
          "Binomiale négative",
          "Gamma log",
          "Gamma inverse",
          "Lognormale"
        ),
        selected = r$loi
      )
    })

    output$info <- renderText({return(
      i18n$t("Attention si l'intéraction n'a pas d'effet elle est retirée automatiquement du modèle.")
    )
    })

    covariable <- reactive({
      if (is.null(r$data_analyse)){return()}
      vector <- colnames(r$data_analyse[,2:length(colnames(r$data_analyse))])
      cov <- c()
      # we check the variable already in the formula
      for (variable in vector) {
        if(grepl(variable, as.character(ecriture()[[1]])) == FALSE){
          cov <- c(cov,variable)
        }
      }
      cov
    })

    #output$test <- renderPrint({ecriture()[[3]]})

    output$covariable <-
      renderUI({
        if(input$methode == 1){
          selectInput(
            ns("covariable"),
            i18n$t("Ajouter une covariable ?"),
            choices = c("year"),
            selected = F,
            multiple = T
          )
        }else{
          selectInput(
            ns("covariable"),
            i18n$t("Ajouter une ou plusieurs covariables ?"),
            choices = c("year","station","campagne"),#covariable(),
            selected = F,
            multiple = T
          )
        }
      })

    # Mise en forme des expression reactives
    y_variable <- reactive({
      input$y
    })
    data_complet <- reactive({
      r$data_analyse
    })

    colonne_y <-
      reactive({
        unlist(data_complet()[, y_variable()])
      }) # sinon ggplot ne marche pas pour l'histogramme

    observe({
      r$interaction <- input$interaction
    })
    observe({
      r$methode <- input$methode
    })
    observe({
      r$distribution <- input$distribution
    })
    observe({
      r$covariable <- input$covariable
    })

    #ecriture du modèle initial

    ecriture <-
      reactive({
        if(is.null(data_complet())){return()}
        if(is.null(input$distribution)){return()}
        if(r$methode == 3){
          return( ecriture_modele(y_variable(),
                          input$interaction,
                          input$methode,
                          input$covariable,
                          input$distribution)
          )
        } else if(input$distribution == "Lognormale"){
          return(
            ecriture_modele_log(y_variable(),
                                input$interaction,
                                input$methode,
                                input$covariable,
                                input$distribution)
          )
        } else{
        ecriture_modele(y_variable(),
                        input$interaction,
                        input$methode,
                        input$covariable,
                        input$distribution)}
      })
    output$ecriture_modele <- renderText(ecriture()[[1]])
    #output$ecriture_distribution <- renderText(ecriture()[[2]])

    observe({
      r$ecriture <- ecriture()
    })

    observeEvent( input$go2, {
      r$go2 <- TRUE
    })

    #Afficher le choix du modèle entre initial et final seulement si l'interaction n'est pas significative (et donc retirée)

    output$choix_modele <- renderUI({
      if(is.null(r$modele)){return()}
      if(class(r$modele) == "try-error"){return()}
      if (r$methode == 3){
        selectInput(
          ns("choix_modele"),
          i18n$t("Afficher les résultats du modèle :"),
          c("initial" = "1", "final" = "2"),
          selected = "2"
        )
      } else if(is.null(getCall(r$modele[[2]]))){
        return()
      } else if (getCall(r$modele[[2]]) != getCall(r$modele[[1]])) {
        selectInput(
          ns("choix_modele"),
          i18n$t("Afficher les résultats du modèle :"),
          c("initial" = "1", "final" = "2"),
          selected = "2"
        )
      } else {
        return()
      }
    })

    choix_modele <- reactive({
      if (!is.null(input$choix_modele)) {
        as.integer(input$choix_modele)
      } else {
        2
      }
    })

    observe({
      r$choix_modele <- choix_modele()
    })

    output$go_represent <- renderUI({
      if(is.null(r$modele)){return()}
      actionButton("go_represent", i18n$t("Voir les effets"), icon = icon("ship"))
    })

    ### check point###
    #output$test <- renderPrint({
    #  ecriture()[[3]]
    #})



}

## To be copied in the UI
# mod_Prepare_modelling_ui("Prepare_modelling_1")

## To be copied in the server
# mod_Prepare_modelling_server("Prepare_modelling_1")
