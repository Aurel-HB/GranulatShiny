#' Indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Indicator_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("En cours de dèveloppement"),
    actionButton("struct", "Structure de la communeauté")
  )
}

#' Indicator Server Functions
#'
#' @noRd
mod_Indicator_server <- function (input, output, session, r){
  ns <- session$ns

}

## To be copied in the UI
# mod_Indicator_ui("Indicator_1")

## To be copied in the server
# mod_Indicator_server("Indicator_1")
