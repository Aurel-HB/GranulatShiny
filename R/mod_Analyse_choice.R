#' Analyse_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Analyse_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    #plotOutput(ns('plot_y')),
    plotOutput(ns('plot_loi')),
    verbatimTextOutput(ns('ntest')),
    verbatimTextOutput(ns('vecteur_loi'))
  )
}

#' Analyse_choice Server Functions
#'
#' @noRd
mod_Analyse_choice_server <- function(input, output, session, r){
    ns <- session$ns

    # Mise en forme des expression reactives
    y_variable <- reactive({
      r$var_name
    })
    data_complet <- reactive({
      r$data_analyse
    })

    colonne_y <-
      reactive({
        unlist(data_complet()[, y_variable()])
      }) # sinon ggplot ne marche pas pour l'histogramme

    #Ggplot pour visualiser la variable Y
    #output$plot_y <-
    #  renderPlot({
    #    ggplot(data = data_complet(), aes(x = colonne_y(), y = after_stat(density))) +
    #      geom_histogram(colour = "black", fill = "grey") +
    #      geom_density(alpha = .2, fill = "blue") +
    #      xlab(y_variable()) +
    #      theme_minimal()
#
    #  })

    output$plot_loi <- renderPlot({
      if (is.null(data_complet())){return()}
      ggplot(data = data_complet(), aes(x = colonne_y(), y = after_stat(density))) +
        geom_histogram(colour = "black", fill = "grey", bins = 100) +
        geom_density(alpha = .3, fill = "blue") +
        geom_area(aes( y= probability_distribution(colonne_y(), r$loi)),
                  color="darkslategray", fill = "darkseagreen",
                  alpha = 0.4, linewidth = 1)+
        xlab(y_variable()) +
        theme_minimal()

    })

    output$ntest <- renderPrint({
      colonne_y()
    })

    output$vecteur_loi <- renderPrint({
      if (is.null(data_complet())){return()}
      vector <- probability_distribution(colonne_y(), r$loi)
      if (is.null(vector)){return()}
      #if (!exists("vector")){return()}
      #if (length(vector)==0){return()}
      verif <- 0

      for (value in is.na(vector)){
        if (value == TRUE){
          verif <- verif + 1
        }
      }

      if (r$loi == "Binomiale"){
        for (value in na.omit(vector)){
          if (value == 0){
            verif <- verif + 1
          }
        }
        if (verif != length(vector)){
          #there other thing than zero and NA so
          verif = 0
        }
      }

      if (!r$loi == "Binomiale"){
        for (value in na.omit(vector)){
          if (value == 0){
            verif <- verif + 1
          }
        }
      }

      if(verif > length(vector)/2){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = "La loi de probabilité choisie n'est pas appropriée !
          Veuillez changer de loi.",
          type = "fail"
        )
        return()
        }
      vector
    })


}

## To be copied in the UI
# mod_Analyse_choice_ui("Analyse_choice_1")

## To be copied in the server
# mod_Analyse_choice_server("Analyse_choice_1")
