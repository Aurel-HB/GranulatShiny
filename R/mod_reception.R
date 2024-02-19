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
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  #i18n$use_js() ### <<-- Add this

  ns <- NS(id)

  tagList(
    h1(i18n$t("Bienvenue sur l'application GranulatShiny")),
    sidebarPanel(
      box( solidHeader = F,
                      collapsible = F,
                      width = "100%",
                      textOutput(ns("author")),
                      br(),
                      actionButton(ns("guide"),
                                   i18n$t("Guide utilisateur"),
                                   icon = icon("book")),
                      br(),
         h5(i18n$t("Lien URL vers les documents de références")),
                      uiOutput(ns("ref_file_url_1")),
                      uiOutput(ns("ref_file_url_2")),
                      uiOutput(ns("ref_file_url_3")),
                      uiOutput(ns("ref_file_url_4")),
                      uiOutput(ns("ref_file_url_5")),
                      hr(),
         h5(i18n$t("Lien URL vers les documents d'informations")),
         uiOutput(ns("inf_file_url_1")),
         uiOutput(ns("inf_file_url_2")),
         uiOutput(ns("inf_file_url_3")),
         uiOutput(ns("inf_file_url_4")),
         uiOutput(ns("inf_file_url_5")),
         uiOutput(ns("inf_file_url_6")),
         hr(),
                      actionButton("start", "start", icon = icon("ship"))
                 ),
      br()),
    #img(src='www/favicon.png', align = "left", style = "width: 234px; height: 260px"),
    mainPanel(
      tags$style(HTML("
      /* Define CSS styling for the box */
      .scroll-box {
        height: 50vh; /* Set height to 50% of the viewport height */
        overflow-y: auto; /* Enable vertical scrolling */
        padding: 10px; /* Add padding */
      }
    ")),
      img(src='www/favicon.png', align = "left", style = "width: 234px; height: 260px"),
    box( solidHeader = T,
         collapsible = F,
         title = i18n$t("Cadre pour l'extraction de granulats marins"),
         status = "warning",
         class = "scroll-box",  # Add class to apply custom styling
         textOutput(ns("intro")))
  ))
}

#' reception Server Functions
#'
#' @noRd
mod_reception_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  #i18n$use_js() ### <<-- Add this
  #i18n doesn't work with renderText if the the expression is not in a return
  #outside a box i18n doesn't work with renderText



  ns <- session$ns

    output$author <- renderText({
      aut <- i18n$t("Auteurs")
      return(paste(aut,": Aurel Hebert--Burggraeve, Mathis Cambreling, Jehanne Rivet,
      Laure Simplet, Vincent Badts, Laurent Dubroca, Camille Vogel", sep=""))
    })

    #observeEvent(input$guide, {
    #  browseURL(paste("file://", "www/apps_guide.html", sep=""))
    #})

    #"URL link of reference document: ",
    output$ref_file_url_1 <- renderUI({
      url <- a("CRR_WGEXT_(1)", href="https://ices-library.figshare.com/articles/report/Effects_of_extraction_of_marine_sediments_on_the_marine_environment_2005-2011/18624086")
      tagList(url)
    })
    output$ref_file_url_2 <- renderUI({
      url <- a("DOGGM_(2)", href="https://side.developpement-durable.gouv.fr/PACA/doc/SYRACUSE/385733/guide-methodologique-pour-l-elaboration-des-documents-d-orientations-pour-une-gestion-durable-des-gr")
      tagList(url)
    })
    output$ref_file_url_3 <- renderUI({
      url <- a("Protocole_halieutique_(3)", href="https://www.geo-ocean.fr/Expertise/Appui-a-la-Puissance-Publique/Les-granulats-marins/Granulats-marins/Protocoles/Ressources-halieutiques")
      tagList(url)
    })
    output$ref_file_url_4 <- renderUI({
      url <- a("Guide_technique_etude_impact_(4)", href="https://www.mineralinfo.fr/fr/actualite/actualite/elaboration-des-etudes-dimpact-granulats-marins-guide-technique-2023")
      tagList(url)
    })


    #"URL link of information document: ",
    output$inf_file_url_1 <- renderUI({
      url <- a("Scientific_report_WGEXT_(5)", href="https://ices-library.figshare.com/articles/report/Working_Group_on_the_Effects_of_Extraction_of_Marine_Sediments_on_the_Marine_Ecosystem_WGEXT_/18621728/1")
      tagList(url)
    })
    output$inf_file_url_2 <- renderUI({
      url <- a("Economie_bleue_granulats_marins_(6)", href="https://www.gouvernement.fr/sites/default/files/contenu/piece-jointe/2023/04/09-l-economie-bleue-en-france-2022-granulats-marins.pdf")
      tagList(url)
    })
    output$inf_file_url_3 <- renderUI({
      url <- a("UNPG_sables_graviers_mer_(7)", href="https://sablesetgraviersenmer.fr/")
      tagList(url)
    })
    output$inf_file_url_4 <- renderUI({
      url <- a("Expertise_granulats_marins_(8)", href="https://www.geo-ocean.fr/Expertise/Appui-a-la-Puissance-Publique/Les-granulats-marins/Granulats-marins")
      tagList(url)
    })
    output$inf_file_url_5 <- renderUI({
      url <- a("Impact_exploitation_granulats_marins_(9)", href="https://www.vie-publique.fr/rapport/126797-exploration-ou-exploitation-des-ressources-minerales-marines")
      tagList(url)
    })
    output$inf_file_url_6 <- renderUI({
      url <- a("Cartographie_ressources_minérales_(10)", href="https://sextant.ifremer.fr/granulats-marins/Ressources-minerales")
      tagList(url)
    })




    # Output the intro text
    output$intro <- renderText({
      if(is.null(r$lang)){return()}
      else if(r$lang == "fr"){
        return(intro_text_fr)
      }
      else {#(r$lang == "en"){
        return(intro_text_en)
      }
    })

}

## To be copied in the UI
# mod_reception_ui("reception_1")

## To be copied in the server
# mod_reception_server("reception_1")
