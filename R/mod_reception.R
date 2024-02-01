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
    textOutput(ns("ref")),
    uiOutput(ns("ref_file_url_1")),
    uiOutput(ns("ref_file_url_2")),
    uiOutput(ns("ref_file_url_3")),
    uiOutput(ns("ref_file_url_4")),
    #uiOutput(ns("ref_file_url_5")),
    hr(),
    actionButton("start", "start", icon = icon("ship"))
    )
  )
}

#' reception Server Functions
#'
#' @noRd
mod_reception_server <- function(input, output, session, r){
    ns <- session$ns

    output$author <- renderText({
      "developed by Aurel Hebert--Burggraeve, Mathis Cambreling, Jehanne Rivet,
      Laure Simplet, Vincent Badts, Laurent Dubroca, Camille Vogel"
    })

    observeEvent(input$guide, {
      browseURL(paste("file://", "www/apps_guide.html", sep=""))
    })

    output$ref <- renderText({
      "Lien URL vers les documetns de references"
    })
    #"URL link of reference document: ",
    output$ref_file_url_1 <- renderUI({
      url <- a("WGEXT", href="https://ices-library.figshare.com/articles/report/Working_Group_on_the_Effects_of_Extraction_of_Marine_Sediments_on_the_Marine_Ecosystem_WGEXT_/18621728/1")
      tagList(url)
    })
    output$ref_file_url_2 <- renderUI({
      url <- a("DOGGM", href="https://side.developpement-durable.gouv.fr/PACA/doc/SYRACUSE/385733/guide-methodologique-pour-l-elaboration-des-documents-d-orientations-pour-une-gestion-durable-des-gr")
      tagList(url)
    })
    output$ref_file_url_3 <- renderUI({
      url <- a("Protocole_halieutique", href="https://www.geo-ocean.fr/Expertise/Appui-a-la-Puissance-Publique/Les-granulats-marins/Granulats-marins/Protocoles/Ressources-halieutiques")
      tagList(url)
    })
    output$ref_file_url_4 <- renderUI({
      url <- a("Guide_technique", href="https://www.mineralinfo.fr/fr/actualite/actualite/elaboration-des-etudes-dimpact-granulats-marins-guide-technique-2023")
      tagList(url)
    })
    #output$ref_file_url_5 <- renderUI({
    #  url <- a("", href="")
    #  tagList("URL link: ", url)
    #})

}

## To be copied in the UI
# mod_reception_ui("reception_1")

## To be copied in the server
# mod_reception_server("reception_1")
