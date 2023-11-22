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
  ns <- NS(id)
  tagList(
    #Input des glmms
      uiOutput(ns("variable_y")),
      selectInput(
        ns("methode"),
        "Sélectionnez la méthode statistique",
        c(
          "GLMM" = "1",
          "GLM" = "2",
          "Permanova" = "3"
        )
      ),
      conditionalPanel(
        "input.methode != 3",
        selectInput(
          ns("loi"),
          "Sélectionnz la loi de distribution",
          c(
            "Normale",
            "Binomiale" = "binomial",
            "Poisson" = "poisson",
            "Binomiale négative",
            "Gamma log",
            "Gamma inverse",
            "Lognormale"
          )
        )
      ),
      checkboxInput(ns("interaction"), "Voulez-vous retirer l'interaction ?"),
      uiOutput(ns("covariable")),

      hr(),
      h4(strong("Formulation du modèle :")),
      htmlOutput(ns("ecriture_modele")),
      htmlOutput(ns("ecriture_loi")),
      verbatimTextOutput(ns("test")),
      hr(),
      actionButton(ns("go2"), "Lancer la modélisation"),
      uiOutput(ns("choix_modele")),
      conditionalPanel(
        "input.go2 != false",
        selectInput(
          ns("choix_sortie"),
          "Afficher les sorties :",
          c(
            "Anova" = "1",
            "Summary" = "2",
            "Vérification" = "3"
          ),
          selected = "1"
        )
      )





  )
}

#' Prepare_modelling Server Functions
#'
#' @noRd
mod_Prepare_modelling_server <- function(input, output, session, r){
    ns <- session$ns
    # GLMMs -------------------------------------------------------------------
    # Onglet Creation

    # Render UI parametrage glmms

    output$variable_y <-
      renderUI({
        selectInput(ns("y"), "Choississez la variable Y", choices = r$var_name)
      })

    output$covariable <-
      renderUI({
        selectInput(
          ns("covariable"),
          "Ajouter une ou plusieurs covariables ?",
          choices = colnames(r$data_analyse[,2:6]),
          selected = F,
          multiple = T
        )
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

    interaction <- reactive({
      input$interaction
    })
    methode <- reactive({
      input$methode
    })
    loi <- reactive({
      input$loi
    })


    #ecriture du modèle initial

    ecriture <-
      reactive({
        if(is.null(data_complet())){return()}
        if(loi() == "Lognormale"){
          return(
            ecriture_modele_log(y_variable(),
                            input$interaction,
                            input$methode,
                            input$covariable,
                            input$loi)
          )
        }
        ecriture_modele(y_variable(),
                        input$interaction,
                        input$methode,
                        input$covariable,
                        input$loi)
      })
    output$ecriture_modele <- renderText(ecriture()[[1]])
    output$ecriture_loi <- renderText(ecriture()[[2]])

    observe({
      r$ecriture <- ecriture()
    })

    observe({
      r$modele <- NULL
    })
    #Afficher le choix du modèle entre initial et finalseulement si l'interaction n'est pas significative (et donc retirée)

    output$choix_modele <- renderUI({
      if(is.null(r$modele)){return()}
      if (getCall(modele()[[2]]) != getCall(modele()[[1]])) {
        selectInput(
          ns("choix_modele"),
          "Afficher les résultats du modèle :",
          c("initial" = "1", "final" = "2"),
          selected = "2"
        )
      }
    })

    choix_modele <- reactive({
      if (!is.null(input$choix_modele)) {
        as.integer(input$choix_modele)
      } else {
        2
      }
    })

    output$test <- renderPrint({
      ecriture()[[3]]
    })

}

## To be copied in the UI
# mod_Prepare_modelling_ui("Prepare_modelling_1")

## To be copied in the server
# mod_Prepare_modelling_server("Prepare_modelling_1")
