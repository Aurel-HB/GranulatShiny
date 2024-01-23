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
         verbatimTextOutput(ns("modele")),
         downloadButton(ns("downloadModel"),
                        label = "Telecharger le modele"),
         downloadButton(ns("downloadSummary"),
                        label = "Telecharger le summary")
    ),
      #plot de vérification
    box(
      plotOutput(ns("verification")),
      downloadButton(ns("downloadPlot"),
                     label = "Telecharger le graphique"),
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

    ## Exporter le modele
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste("model",".rds", sep = "")
      },
      content = function(file) {
        # Use tryCatch to handle errors with try(silent = TRUE)
        tryCatch(
          {
            base::saveRDS(modele()[[choix_modele()]], file)
          },
          error = function(e) {
            # Handle the error here (print a message, log it, etc.)
            print("")
          },
          warning = function(w) {
            # Handle warnings if needed
            print("")
          }
        )
      }, contentType = "application/x-rds")



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
        selected = "2"
      )
    })

    observe({
      r$choix_sortie <- input$choix_sortie
    })


    observeEvent(r$go2,{
      output$modele <- renderPrint({
        if(is.null(r$choix_sortie)){return()}
        if(class(modele()) == "try-error"){return("Il y a une erreur lors de la modélisation. Veuillez changer la loi ou le modèle.")}
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

    ## exporter le summary
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("modele_summary",".txt", sep = "")
      },
      content = function(file) {
        # Use tryCatch to handle errors with try(silent = TRUE)
        tryCatch(
          {
            # Open a connection to a file (e.g., "output.txt")
            sink(file)

            # Write the content of the text area to the file
            print(summary(modele()[[choix_modele()]]))

            # Close the connection to the file
            sink()
          },
          error = function(e) {
            # Handle the error here (print a message, log it, etc.)
            print("")
          },
          warning = function(w) {
            # Handle warnings if needed
            print("")
          }
        )
      })


    # Vérification (sortie du modèle, même input)
    observeEvent(r$go2,{
      residual <- reactive({
        if (is.null(r$choix_sortie)){return()}
        if (methode()== 3){return()}
        if(class(modele()) == "try-error"){return("Il y a une erreur lors de la modélisation")}
        #plotQQunif(simulateResiduals(modele()[[choix_modele()]]))
        plot(simulateResiduals(modele()[[choix_modele()]]))
      })
      output$verification <- renderPlot({
        residual()
      })
    })

    ## Exporter le graphique
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot_residual",".png", sep = "")
      },
      content = function(file) {
        # Use tryCatch to handle errors with try(silent = TRUE)
        tryCatch(
          {
           #open the device
            png(file,height = 600, width = 1070)
            #create the plot
            plot(simulateResiduals(modele()[[choix_modele()]]))
            #close the device
            dev.off()
          },
          error = function(e) {
            # Handle the error here (print a message, log it, etc.)
            print("")
          },
          warning = function(w) {
            # Handle warnings if needed
            print("")
          }
        )
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
