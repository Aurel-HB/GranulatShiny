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
           uiOutput(ns("guide_user")),
                      br(),
           box( solidHeader = T,
                collapsible = T,
                collapsed = T,
                width = "100%",
                title = i18n$t("Lien URL vers les documents de références"),
                uiOutput(ns("ref_file_url_1")),
                uiOutput(ns("ref_file_url_2")),
                uiOutput(ns("ref_file_url_3")),
                uiOutput(ns("ref_file_url_4")),
                uiOutput(ns("ref_file_url_5"))),
                      hr(),
        box( solidHeader = T,
             collapsible = T,
             collapsed = T,
             width = "100%",
             title = i18n$t("Lien URL vers les documents d'informations"),
             uiOutput(ns("inf_file_url_1")),
             uiOutput(ns("inf_file_url_2")),
             uiOutput(ns("inf_file_url_3")),
             uiOutput(ns("inf_file_url_4")),
             uiOutput(ns("inf_file_url_5")),
             uiOutput(ns("inf_file_url_6"))),
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
         collapsible = T,
         collapsed = T,
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

    #output$guide_user <- renderUI({
    #  url <- a(i18n$t("Guide utilisateur"), href="https://raw.githack.com/Aurel-HB/GranulatShiny/main/Guide_utilisateur.html")
    #  tagList(icon('book'),url)
    #})

    output$guide_user <- renderUI({
      if(is.null(r$lang)){return()}
      else if(r$lang == "fr"){
        url <- a(
          icon('book', style = "font-size: 25px;"),
          "Guide utilisateur",
          href = "https://raw.githack.com/Aurel-HB/GranulatShiny/main/Guide_utilisateur.html",
          style = "font-size: 20px;"
        )
      }
      else {
        url <- a(
          icon('book', style = "font-size: 25px;"),
          "User guide",
          href = "https://raw.githack.com/Aurel-HB/GranulatShiny/main/Guide_utilisateur.html",
          style = "font-size: 20px;"
        )
      }
      tagList(url)
    })

    #"URL link of reference document: ",
    output$ref_file_url_1 <- renderUI({
      url <- a("(1) CRR WGEXT", href="https://ices-library.figshare.com/articles/report/Effects_of_extraction_of_marine_sediments_on_the_marine_environment_2005-2011/18624086")
      tagList(url)
    })
    output$ref_file_url_2 <- renderUI({
      url <- a("(2) DOGGM", href="https://side.developpement-durable.gouv.fr/PACA/doc/SYRACUSE/385733/guide-methodologique-pour-l-elaboration-des-documents-d-orientations-pour-une-gestion-durable-des-gr")
      tagList(url)
    })
    output$ref_file_url_3 <- renderUI({
      url <- a("(3) Protocole halieutique", href="https://www.geo-ocean.fr/Expertise/Appui-a-la-Puissance-Publique/Les-granulats-marins/Granulats-marins/Protocoles/Ressources-halieutiques")
      tagList(url)
    })
    output$ref_file_url_4 <- renderUI({
      url <- a("(4) Guide technique étude impact", href="https://www.mineralinfo.fr/fr/actualite/actualite/elaboration-des-etudes-dimpact-granulats-marins-guide-technique-2023")
      tagList(url)
    })


    #"URL link of information document: ",
    output$inf_file_url_1 <- renderUI({
      url <- a("(5) Scientific report WGEXT", href="https://ices-library.figshare.com/articles/report/Working_Group_on_the_Effects_of_Extraction_of_Marine_Sediments_on_the_Marine_Ecosystem_WGEXT_/18621728/1")
      tagList(url)
    })
    output$inf_file_url_2 <- renderUI({
      url <- a("(6) Economie bleue granulats marins", href="https://www.info.gouv.fr/organisation/secretariat-general-de-la-mer-sgmer/l-economie-bleue-en-france")
      tagList(url)
    })
    output$inf_file_url_3 <- renderUI({
      url <- a("(7) UNPG sables graviers mer", href="https://sablesetgraviersenmer.fr/")
      tagList(url)
    })
    output$inf_file_url_4 <- renderUI({
      url <- a("(8) Expertise granulats marins", href="https://www.geo-ocean.fr/Expertise/Appui-a-la-Puissance-Publique/Les-granulats-marins/Granulats-marins")
      tagList(url)
    })
    output$inf_file_url_5 <- renderUI({
      url <- a("(9) Impact exploitation granulats marins", href="https://www.vie-publique.fr/rapport/126797-exploration-ou-exploitation-des-ressources-minerales-marines")
      tagList(url)
    })
    output$inf_file_url_6 <- renderUI({
      url <- a("(10) Cartographie ressources minérales", href="https://sextant.ifremer.fr/granulats-marins/Ressources-minerales")
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
