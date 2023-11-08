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
    plotOutput(ns('plot_y'))
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
    output$plot_y <-
      renderPlot({
        ggplot(data = data_complet(), aes(x = colonne_y())) +
          geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
          geom_density(alpha = .2, fill = "blue") +
          xlab(y_variable()) +
          theme_minimal()
      })

}

## To be copied in the UI
# mod_Analyse_choice_ui("Analyse_choice_1")

## To be copied in the server
# mod_Analyse_choice_server("Analyse_choice_1")
