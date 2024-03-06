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
    plotOutput(ns('plot_loi'), width = "100%"),
    #verbatimTextOutput(ns('ntest')),
    textOutput(ns('vecteur_loi'))
  )
}

#' Analyse_choice Server Functions
#'
#' @noRd
mod_Analyse_choice_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
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

    })#, height = 750, width = 1100)

    #output$ntest <- renderPrint({
    #  colonne_y()
    #})

    output$vecteur_loi <- renderText({
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
          #there is other thing than zero and NA so
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

      if(verif > length(vector)*0.8){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = i18n$t("La loi de probabilité choisie n'est pas appropriée ! Veuillez changer de loi."),
          type = "fail"
        )
        return()
        }
      #vector
    })


}

## To be copied in the UI
# mod_Analyse_choice_ui("Analyse_choice_1")

## To be copied in the server
# mod_Analyse_choice_server("Analyse_choice_1")
