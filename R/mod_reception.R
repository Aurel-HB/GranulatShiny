#' reception UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,r Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reception_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Bienvenue sur l'application GranulatShiny"),
    textOutput(ns("author"))
  )
}

#' reception Server Functions
#'
#' @noRd
mod_reception_server <- function(input, output, session, r){
    ns <- session$ns

    output$author <- renderText({
      "by Aurel Hebert--Burggraeve, Mathis Cambreling, Jehanne Rivet, Laurent Dubroca, Camille Vogel"
    })

}

## To be copied in the UI
# mod_reception_ui("reception_1")

## To be copied in the server
# mod_reception_server("reception_1")
