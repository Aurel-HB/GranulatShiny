#' Prepare_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Prepare_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    box( #title = "Variable",
         solidHeader = TRUE,
         collapsible = TRUE,
         width = 12,
         textOutput(ns("variable_y"))),
    selectInput(
      ns("methode"),
      "Sélectionnez la méthode statistique",
      c(
        "GLMM" = "1",
        "GLM" = "2",
        "Permanova" = "3"
      )),
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
          "Gamma inverse"
        )
      )
    ),
    checkboxInput(ns("interaction"), "Voulez-vous retirer l'interaction ?"),
    uiOutput(ns("covariable")),
    hr(),
    h4(strong("Formulation du modèle :")),
    htmlOutput(ns("ecriture_modele")),
    htmlOutput(ns("ecriture_loi")),
    hr()
  )
}

#' Prepare_model Server Functions
#'
#' @noRd
mod_Prepare_model_server <- function(input, output, session, r){
    ns <- session$ns

    data_analyse <- reactive({
      r$data_analyse
    })


    # Render UI parametrage glmms

    output$variable_y <- renderText({
      r$var_name
    })

    output$covariable <-
      renderUI({
        selectInput(
          "covariable",
          "Ajouter une ou plusieurs covariables ?",
          choices = names(data_analyse())[2:length(names(data_analyse()))],
          selected = F,
          multiple = T
        )
      })

    # Mise en forme des expression reactives
    y_variable <- reactive({
      r$var_name
    })
    data_complet <- reactive({
      data_analyse()
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
    #Ggplot pour visualiser la variable Y
    #output$plot_y <-
    #  renderPlot({
    #    ggplot(data = data_complet(), aes(x = colonne_y())) +
    #      geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
    #      geom_density(alpha = .2, fill = "blue") +
    #      xlab(y_variable()) +
    #      theme_minimal()
    #  })

    #ecriture du modèle initial

    ecriture <-
      reactive({
        ecriture_modele(y_variable(),
                        input$interaction,
                        input$methode,
                        input$covariable,
                        input$loi)
      })

    output$ecriture_modele <- renderText(ecriture()[[1]])
    output$ecriture_loi <- renderText(ecriture()[[2]])
    # formule et language modèle
    formule <- reactive({
      ecriture()[[3]]
    })
    formule_bis <- reactive({
      ecriture()[[4]]
    }) # formule sans interaction
    language <-
      reactive({
        ecriture()[[5]]
      }) #permet de bien nommer les variabes pour les GLM, necessaire pour que les test de puissances marchent
    language_bis <- reactive({
      ecriture()[[6]]
    })
    formule_interaction <- reactive({
      ecriture()[[7]]
    })
    language_interaction <- reactive({
      ecriture()[[8]]
    })
}

## To be copied in the UI
# mod_Prepare_model_ui("Prepare_model_1")

## To be copied in the server
# mod_Prepare_model_server("Prepare_model_1")
