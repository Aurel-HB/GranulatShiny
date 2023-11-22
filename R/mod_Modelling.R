#' Modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Modelling_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Output des glmms
      #formulation du modèle
      verbatimTextOutput(ns("modele")),
      #plot de vérification
      conditionalPanel("input.choix_sortie == 3", plotOutput(ns("verification")))
  )
}

#' Modelling Server Functions
#'
#' @noRd
mod_Modelling_server <- function(input, output, session, r){
    ns <- session$ns

    # GLMMs -------------------------------------------------------------------
    # Onglet Creation ####
    ######################

    #import des objets utiles dans le modules
    data_complet <- reactive({
      r$data_analyse
    })

    ecriture <- reactive({
      r$ecriture
    })


    # formule et language modèle
    formule <- reactive({
      if(is.null(ecriture())){return()}
      ecriture()[[3]]
    })
    formule_bis <- reactive({
      if(is.null(ecriture())){return()}
      ecriture()[[4]]
    }) # formule sans interaction
    language <-
      reactive({
        if(is.null(ecriture())){return()}
        ecriture()[[5]]
      }) #permet de bien nommer les variabes pour les GLM, necessaire pour que les test de puissances marchent
    language_bis <- reactive({
      if(is.null(ecriture())){return()}
      ecriture()[[6]]
    })
    formule_interaction <- reactive({
      if(is.null(ecriture())){return()}
      ecriture()[[7]]
    })
    language_interaction <- reactive({
      if(is.null(ecriture())){return()}
      ecriture()[[8]]
    })

    #Modélo différentes méthodes
    modele <- eventReactive(input$go2, {
      if (input$methode == "1") {
        glmm_maker(data_complet(),
                   formule(),
                   formule_bis(),
                   input$interaction,
                   input$loi)
      } else if (input$methode == "2")  {
        glm_maker(
          data_complet(),
          formule(),
          formule_bis(),
          language(),
          language_bis(),
          input$interaction,
          input$loi
        )
      } else if (input$methode == "3") {
        permanova_maker(data_complet(),
                        formule(),
                        formule_bis(),
                        input$interaction)
      }
    })



}

## To be copied in the UI
# mod_Modelling_ui("Modelling_1")

## To be copied in the server
# mod_Modelling_server("Modelling_1")
