#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import shiny
#' @import shinyWidgets
#' @import tidyr
#' @import vegan
#' @import leaflet
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @import lme4
#' @import car
#' @import stats
#' @import MASS
#' @import DHARMa
#' @import stringr
#' @import multcomp
#' @import multcompView
#' @import lsmeans
#' @import RColorBrewer
#' @import plyr
#' @import sf
#' @import ggeffects
#' @import cowplot
#' @import DT
#'
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # reactiveValues créée pour sauvegarder les infos des modules
  r <- reactiveValues()

  #calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")

  # onglet accueil
  callModule(mod_reception_server, id = "reception_1",session = session, r=r)
  # onglet import data
  callModule(mod_Import_data_server, id = "Import_data_1",session = session, r=r)
  callModule(mod_Ref_server, id = "Ref_1",session = session, r=r)
  callModule(mod_Impact_server, id = "Impact_1",session = session, r=r)
  callModule(mod_Map_server, id = "Map_1",session = session, r=r)
  # onglet table
  callModule(mod_Prepare_data_server, id = "Prepare_data_1",session = session, r=r)
  callModule(mod_Tables_server, id = "Tables_1",session = session, r=r)
  callModule(mod_Show_tables_server, id = "Show_tables_1", session =session, r=r)
  #onglet plot indicator
  callModule(mod_Indicator_server, id = "Indicator_1", session =session, r=r)
  #onglet plot structure
  callModule(mod_Structure_server, id = "Structure_1", session =session, r=r)
  #onglet plot stat
  callModule(mod_Stat_server, id = "Stat_1",session = session, r=r)
  #onglet diagnostic analyse
  callModule(mod_Choix_loi_server, id = "Choix_loi_1",session = session, r=r)
  callModule(mod_Analyse_choice_server, id = "Analyse_choice_1",session = session, r=r)
  #onglet modelisation
  callModule(mod_Prepare_modelling_server, id = "Prepare_modelling_1",session = session, r=r)
  callModule(mod_Modelling_server, id = "Modelling_1",session = session, r=r)
  #onglet representation
  callModule(mod_Representation_server, id = "Representation_1",session = session, r=r)
  #onglet puissance statistique
  callModule(mod_Puissance_server, id = "Puissance_1",session = session, r=r)


  #permet de passer à la page des données
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "donnees")
  })
  # Ouvre l'onglet Tables automatiquement
  observeEvent(r$button, {
    updateTabItems(session, "tabs", "table")
    r$button <- FALSE # réinitialise le bouton
  })#}) # pour que ce ça marche l'appel des modules doit se faire avant
  observeEvent(input$gostat1, {
    updateTabItems(session, "tabs", "Indic")
  })
  observeEvent(input$struct, {
    updateTabItems(session, "tabs", "Struc")
  })
  observeEvent(input$descript, {
    updateTabItems(session, "tabs", "plot")
  })
  observeEvent(input$gostat2, {
    updateTabItems(session, "tabs", "plot")
  })
  observeEvent(input$goloi, {
    updateTabItems(session, "tabs", "Diag")
  })
  observeEvent(input$go_modelo, {
    updateTabItems(session, "tabs", "mod")
  })
  observeEvent(input$go_represent, {
    updateTabItems(session, "tabs", "rep")
  })

  # change language
  observeEvent(input$lang, {
    shiny.i18n::update_lang(input$lang,session)
  })
  observe({
    r$lang <- input$lang
  })


}
