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
    mainPanel(
    img(src='www/favicon.png', align = "right", style = "width: 234px; height: 260px"),
    h1("Bienvenue sur l'application GranulatShiny"),
    textOutput(ns("author")),
    br(),
    actionButton(ns("guide"), "Guide utilisateur", icon = icon("book")),
    hr(),
    actionButton("start", "start"),
    #selectInput(ns("mode"),"Sélectionner un mode :",
    #  c("user" = "1","developer" = "2"),
    #  selected = "1"
    #),
    br(),
    checkboxInput(ns("mode"), "Passer en mode developpeur ?")
    )
  )
}

#' reception Server Functions
#'
#' @noRd
mod_reception_server <- function(input, output, session, r){
    ns <- session$ns

    output$author <- renderText({
      "developed by Aurel Hebert--Burggraeve, Mathis Cambreling, Jehanne Rivet, Laurent Dubroca, Camille Vogel"
    })

    observe({
      if(input$mode){return(
        r$mode <- c("dev")
      )}
      r$mode <- c("user")
    })

    observe({
      r$path <- paste(getwd(),"/vignettes", sep ="")
    })

}

## To be copied in the UI
# mod_reception_ui("reception_1")

## To be copied in the server
# mod_reception_server("reception_1")
