#' Structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Structure_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("En cours de dèveloppement"),
    actionButton("descript", "Stat descriptives")
  )
}

#' Structure Server Functions
#'
#' @noRd
mod_Structure_server <- function (input, output, session, r){
    ns <- session$ns

}

## To be copied in the UI
# mod_Structure_ui("Structure_1")

## To be copied in the server
# mod_Structure_server("Structure_1")
