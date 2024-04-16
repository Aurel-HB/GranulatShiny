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
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    box( solidHeader = FALSE,
         status = "info",
         collapsible = F,
         width = NULL,
         textOutput(ns("indique")),
         hr(),
         uiOutput(ns("choix_sortie")),
         # Output des glmms
         #formulation du modèle
         verbatimTextOutput(ns("modele"))%>%
           withSpinner(color = "#009a62", type = 5),
         textOutput(ns("spinner")),
         downloadButton(ns("downloadModel"),
                        label = i18n$t("Telecharger le modele (.rds)")),
         downloadButton(ns("downloadSummary"),
                        label = i18n$t("Telecharger le summary (.txt)"))
    ),
      #plot de vérification
    box(
      #actionButton(ns("info"), "",icon = icon("circle-info")),
      uiOutput(ns("info")),
      uiOutput(ns("choix_box")),
      plotOutput(ns("verification")),
      uiOutput(ns("log")),
      downloadButton(ns("downloadPlot"),
                     label = i18n$t("Telecharger le graphique (.png)")),
      width = NULL,
      style = "overflow-x: scroll;",
      collapsible = T,
      collapsed = T,
      solidHeader = TRUE,
      title = textOutput(ns("title"))#"Residual analysis"
    )
  )
}

#' Modelling Server Functions
#'
#' @noRd
mod_Modelling_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

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


    output$title <- renderText(
      if(r$methode == 3){
        return("Boxplot")
      } else{
        return("Residual analysis")
      }
    )

    output$indique <- renderText({
      i18n$t("L'icone de chargement disparaitra une fois que vous aurez cliquez sur le bouton modélisation et que les calculs seront finis (cela peut prendre du temps).")
    })

    # GLMMs -------------------------------------------------------------------
    # Onglet Creation ####
    ######################

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
        # distance matrix for adonis
        vector <- decostand(data_complet()[r$var_name],"chi.square", MARGIN = 2)
        vector <- data.frame(as.numeric(vector[1,]))
        names(vector) <- r$var_name
        dist <- vegdist(vector, method = "euclidean")
        ##
        try(
          permanova_maker(data_complet(),
                          language(),
                          language_bis(),
                        r$interaction,
                        dist),
          silent = T)
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
        i18n$t("Afficher les sorties :"),
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
        if(class(modele()) == "try-error"){return(i18n$t("Il y a une erreur lors de la modélisation. Veuillez changer la loi ou le modèle."))}
        if (methode() %in% c(1, 2)) {
          if (r$choix_sortie == "1") {
            Anova(modele()[[choix_modele()]], type = "III")
          } else if (r$choix_sortie == "2") {
            summary(modele()[[choix_modele()]])
          }


        } else {
          if (r$choix_sortie == "1") {
            #modele()[[choix_modele()]]$aov.tab
            modele()[[choix_modele()]]
          } else if (r$choix_sortie == "2") {
            modele()[[choix_modele()]]
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
            # Open a connection to a file (e.g., "output.txt")
            sink(file)

            # Write the content of the text area to the file
            if(methode()== 3){
              print(modele()[[choix_modele()]])
            } else {
              print(summary(modele()[[choix_modele()]]))
            }

            # Close the connection to the file
            sink()
      })


    #### boxplot part / this part is only for permanova ####
    output$choix_box <- renderUI({
      if(methode()!=3){return()}
      selectInput(
        ns("choix_box"),
        i18n$t("Sélectionner la covariable :"),
        c("traitement" = "1",
          "saison" = "2",
          "traitement:saison" = "3"),
        selected = "1"
      )
    })

    # log choice
    output$log <- renderUI({
      if(methode()!=3){return()}
      checkboxInput(ns("log"), i18n$t("Voulez-vous passer au log ?"))
    })

    # variable is the vector of the chosen variable with or without transformation
    variable <- reactive({
      if(is.null(data_complet())){return()}
      if (input$log){
        return(log(as.numeric(data_complet()[,1]+1)))
      } else {
        return(as.numeric(data_complet()[,1]))
      }
    })

    create_boxplot <- reactive({
      if(is.null(data_complet())){return()}
      if(is.null(input$choix_box)){return()}
      if (input$log){
        var_name <- paste("log(", r$var_name,")", sep="")
      } else{ var_name <- r$var_name}
      if(input$choix_box == 1){
        p_value <- modele()[[choix_modele()]]["traitement","Pr(>F)"]
        if(p_value<0.05){
          legend <- annotate("text", x = 0.65,
                             y = max(as.numeric(variable())),
                             label = paste("* p = ",p_value, sep = ""),
                             colour = "red", size = 5)
        } else {
          legend <- annotate("text", x = 0.65,
                             y = max(as.numeric(variable())),
                             label = i18n$t("Pas d'effet"),
                             colour = "black", size = 5)
        }
        plot <- ggplot(data_complet(), aes(x = traitement, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by impact", sep=""),
               y = "")+ legend
      }
      if(input$choix_box == 2){
        p_value <- modele()[[choix_modele()]]["saison","Pr(>F)"]
        if(p_value<0.05){
          legend <- annotate("text", x = 0.65,
                             y = max(as.numeric(variable())),
                             label = paste("* p = ",p_value, sep = ""),
                             colour = "red", size = 5)
        } else {
          legend <- annotate("text", x = 0.65,
                             y = max(as.numeric(variable())),
                             label = i18n$t("Pas d'effet"),
                             colour = "black", size = 5)
        }
        plot <- ggplot(data_complet(), aes(x = saison, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by season", sep=""),
               y = "")+ legend
      }
      if(input$choix_box == 3){
        if("traitement:saison" %in% row.names( modele()[[choix_modele()]])){
          p_value <- modele()[[choix_modele()]]["traitement:saison","Pr(>F)"]
          if(p_value<0.05){
            legend <- annotate("text", x = 0.65,
                               y = max(as.numeric(variable())),
                               label = paste("* p = ",p_value, sep = ""),
                               colour = "red", size = 5)
          } else {
            legend <- annotate("text", x = 0.65,
                               y = max(as.numeric(variable())),
                               label = i18n$t("Pas d'effet"),
                               colour = "black", size = 5)
          }
          plot <- ggplot(data_complet(), aes(x = interaction, y = as.numeric(variable())))+
            geom_boxplot()+
            labs(title = paste("Boxplot of ", var_name," by impact:season", sep=""),
                 y = "")+ legend
        }else{ plot <- NULL}
        }
      return(plot)
    })
    #####


    # Vérification (sortie du modèle, même input)
    observeEvent(r$go2,{
      residual <- reactive({
        if (is.null(r$choix_sortie)){return()}
        if (methode()== 3){return(create_boxplot())}
        if(class(modele()) == "try-error"){return(i18n$t("Il y a une erreur lors de la modélisation"))}
        #plotQQunif(simulateResiduals(modele()[[choix_modele()]]))
        plot(simulateResiduals(modele()[[choix_modele()]]))
      })
      output$verification <- renderPlot({
        residual()
      })
    })

    ##### information #####

    #output$title <- renderText(
    #  if(r$methode == 3){
    #    return("Boxplot")
    #  } else{
    #    return("Residual analysis")
    #  }
    #)

    output$info <- renderUI({
      if(r$methode==3){
        actionButton(ns("info2"), "",icon = icon("circle-info"))
      } else {
        actionButton(ns("info"), "",icon = icon("circle-info"))
      }
    })


    observeEvent(input$info,{
      message <- as.character(list_translate[r$lang][11,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info2,{
      message <- as.character(list_translate[r$lang][12,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })


    ## Exporter le graphique ####
    observe({
      if(methode() == 3){
        output$downloadPlot <- downloadHandler(
          filename = function() {
            paste("boxplot_", r$var_name, ".png", sep = "")
          },
          content = function(file) {
            # Use tryCatch to handle errors with try(silent = TRUE)
            tryCatch(
              {
                ggsave(file, plot = create_boxplot(), height = 9, width = 12,
                       bg = "white")
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
      } else {
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
      }
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
