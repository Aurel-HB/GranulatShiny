#' Puissance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Puissance_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Inputs pour parametrer le test de puissance
    #sidebarPanel(
      h1("En cours de développement"),
      textOutput(ns("explain"))
    #),
    #Output du grah
    #mainPanel(
      #plotOutput(ns("plot_puissance")) %>% withSpinner(color = "#006522")

    #)
  )
}

#' Puissance Server Functions
#'
#' @noRd
mod_Puissance_server <- function(input, output, session, r){
    ns <- session$ns

    output$explain <- renderText({
      "L'outil construit par Mathis Cambreling fonctionne seulement pour le jeu
      de données ayant servi de base aux calculs. L'outil n'étant pas
      généralisable, celui-ci a été retiré pour assurer la stabilité actuelle
      de l'application. Un autre outil est en cours de développement."
    })
}

## To be copied in the UI
# mod_Puissance_ui("Puissance_1")

## To be copied in the server
# mod_Puissance_server("Puissance_1")
