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
    sidebarPanel(
      #choix du terme
      uiOutput(ns("puissance_choix")),
      #nombre de campagne à simuler
      numericInput(
        ns("nbr_campagne"),
        "Combien de campagne supplémentaire à simuler ?",
        value = 2,
        min = 1,
        step = 1
      ),
      actionButton(ns("go3"), "GO")

    ),
    #Output du grah
    mainPanel(
      plotOutput(ns("plot_puissance")) %>% withSpinner(color = "#eeee22")

    )
  )
}

#' Puissance Server Functions
#'
#' @noRd
mod_Puissance_server <- function(input, output, session){
    ns <- session$ns

}

## To be copied in the UI
# mod_Puissance_ui("Puissance_1")

## To be copied in the server
# mod_Puissance_server("Puissance_1")
