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
#' @import sjPlot
#' @import stringr
#' @import multcomp
#' @import multcompView
#' @import lsmeans
#' @import simr
#' @import RColorBrewer
#' @import plyr
#' @import sf
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # reactiveValues créée pour sauvegarder les infos des modules
  r <- reactiveValues()


  # onglet import data
  callModule(mod_Import_data_server, id = "Import_data_1",session = session, r=r)
  callModule(mod_Ref_server, id = "Ref_1",session = session, r=r)
  callModule(mod_Impact_server, id = "Impact_1",session = session, r=r)
  callModule(mod_Map_server, id = "Map_1",session = session, r=r)
  # onglet table
  callModule(mod_Prepare_data_server, id = "Prepare_data_1",session = session, r=r)
  callModule(mod_Tables_server, id = "Tables_1",session = session, r=r)
  callModule(mod_Show_tables_server, id = "Show_tables_1", session =session, r=r)
  #onglet plot stat
  callModule(mod_Stat_server, id = "Stat_1",session = session, r=r)
  #onglet diagnostic analyse
  callModule(mod_Choix_loi_server, id = "Choix_loi_1",session = session, r=r)
  callModule(mod_Analyse_choice_server, id = "Analyse_choice_1",session = session, r=r)
  #onglet modelisation
  callModule(mod_Prepare_modelling_server, id = "Prepare_modelling_1",session = session, r=r)
  callModule(mod_Modelling_server, id = "Modelling_1",session = session, r=r)

  # Ouvre l'onglet Tables automatiquement
  observeEvent(r$button, {
    updateTabItems(session, "tabs", "table")
    r$button <- FALSE # réinitialise le bouton
  })#}) # pour que ce ça marche l'appel des modules doit se faire avant
  observeEvent(input$gostat, {
    updateTabItems(session, "tabs", "plot")
  })
  observeEvent(input$goloi, {
    updateTabItems(session, "tabs", "Diag")
  })

  #onglet puissance statistique

  #onglet representation des effets

}
