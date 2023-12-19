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
    box( solidHeader = FALSE,
         status = "info",
         collapsible = FALSE,
         width = NULL,
         uiOutput(ns("choix_sortie")),
         # Output des glmms
         #formulation du modèle
         verbatimTextOutput(ns("modele"))
    ),
      #plot de vérification
    box(
      plotOutput(ns("verification")),
      width = NULL,
      style = "overflow-x: scroll;",
      collapsible = T,
      collapsed = T,
      solidHeader = TRUE,
      title = "Residual analysis"
    )
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

    methode <- eventReactive(r$go2,{
      r$methode
    })

    choix_modele <- reactive({
      r$choix_modele
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
    modele <- eventReactive(r$go2, {
      if (r$methode == "1") {
        try(glmm_maker(data_complet(),
                   formule(),
                   formule_bis(),
                   r$interaction,
                   r$distribution), silent = T)
      } else if (r$methode == "2")  {
        try(glm_maker(
          data_complet(),
          formule(),
          formule_bis(),
          language(),
          language_bis(),
          r$interaction,
          r$distribution), silent = T
        )
      } else if (r$methode == "3") {
        try(permanova_maker(data_complet(),
                        formule(),
                        formule_bis(),
                        r$interaction), silent = T)
      }
    })

    observe({
      r$modele <- modele()
    })


    # Les sorties du modèle

    output$choix_sortie <- renderUI({
      if(is.null(modele())){return()}
      selectInput(
        ns("choix_sortie"),
        "Afficher les sorties :",
        c(
          "Anova" = "1",
          "Summary" = "2"
        ),
        selected = "1"
      )
    })

    observe({
      r$choix_sortie <- input$choix_sortie
    })


    observeEvent(r$go2,{
      output$modele <- renderPrint({
        if(is.null(r$choix_sortie)){return()}
        if(class(modele()) == "try-error"){return("Il y a une erreur lors de la modélisation")}
        if (methode() %in% c(1, 2)) {
          if (r$choix_sortie == "1") {
            Anova(modele()[[choix_modele()]], type = "III")
          } else if (r$choix_sortie == "2") {
            summary(modele()[[choix_modele()]])
          }


        } else {
          if (r$choix_sortie == "1") {
            modele()[[choix_modele()]]$aov.tab
          } else if (r$choix_sortie == "2") {
            modele()[[choix_modele()]]$coefficients
          }


        }
      })
    })



    # Vérification (sortie du modèle, même input)
    observeEvent(r$go2,{
      output$verification <- renderPlot({
        if (is.null(r$choix_sortie)){return()}
        if (methode()== 3){return()}
        if(class(modele()) == "try-error"){return("Il y a une erreur lors de la modélisation")}
        #plotQQunif(simulateResiduals(modele()[[choix_modele()]]))
        plot(simulateResiduals(modele()[[choix_modele()]]))
      })
    })


    # réinitialisation du bouton
    observeEvent(r$go2,{
      r$go2 <- FALSE
    })

}

## To be copied in the UI
# mod_Modelling_ui("Modelling_1")

## To be copied in the server
# mod_Modelling_server("Modelling_1")
