#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyBS
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import shinycssloaders
#' @importFrom DT DTOutput
#'
#'
#' @noRd
app_ui <- function(request) {
  # internationalization
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  i18n$use_js() ### <<-- Add this

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( # create the left part of the interface to choose the page
      skin = "blue",
      dashboardHeader(title = tagList(shiny.i18n::usei18n(i18n),"GranulatShiny")
                      ),
      dashboardSidebar(sidebarMenu(
        id = "tabs",
        menuItem(
          i18n$t("Page d'accueil"),
          tabName = "accueil",
          #icon = icon("far fa-file-lines")
          icon = icon("dungeon")
        ),
        menuItem(
          i18n$t("Mise en forme des données"),
          tabName = "forme",
          icon = icon("wand-magic-sparkles"),
          menuSubItem(i18n$t("Informations à rentrer"), tabName = "donnees"),
          menuSubItem("Tables", tabName = "table")
        ),
        menuItem(
          i18n$t("Statistiques exploratoires"),
          tabName = "exploratory",
          icon = icon("eye"),
          menuSubItem(i18n$t("Plot des indicateurs"), tabName = "Indic"),
          menuSubItem(i18n$t("Plot de la structure"), tabName = "Struc")
        ),
        menuItem(
          i18n$t("Statistiques descriptives"),
          tabName = "descriptive",
          icon = icon("scroll"),
          menuSubItem(i18n$t("Plot des données"), tabName = "plot"),
          menuSubItem(i18n$t("Diagnostic d'analyse"), tabName = "Diag")
        ),
        menuItem(
          i18n$t("Statistiques inférentielles"),
          tabName = "glmm",
          icon = icon("fish-fins"),
          menuSubItem(i18n$t("Création des modèles"), tabName = "mod"),
          menuSubItem(i18n$t("Représentation des effets"), tabName = "rep"),
          menuSubItem(i18n$t("Puissance statistique"), tabName = "pui")

        ),
        tags$li(class="dropdown",
                selectInput(inputId="lang",
                            label=i18n$t("Langue"),
                            choices = i18n$get_languages(),
                            selected = i18n$get_key_translation())
                #icon=icon("flag"),
                #                    class= 'dropdown')
        )
      )),
      dashboardBody( # create the interface for each tab
        fluidPage(
        tabItems(
          #page d'accueil ####
          tabItem(
            tabName = "accueil",
            mod_reception_ui("reception_1")
          ),
          #onglet Information à rentrer
          tabItem(
            tabName = "donnees",
            sidebarPanel(mod_Import_data_ui("Import_data_1"),
                         mod_Prepare_data_ui("Prepare_data_1")
                         )  ,
            mainPanel(
              tabBox(id = "tab",
                tabPanel(i18n$t("Stations d'impact"), mod_Impact_ui("Impact_1")
                         )),
              tabBox(id = "tab_reference",
                     tabPanel(
                       i18n$t("Stations de référence"), mod_Ref_ui("Ref_1")
                     )),
              mod_Map_ui("Map_1")
            )

            ),
          #Onglet Tables
          tabItem(
            tabName = "table",
            sidebarPanel(
              mod_Tables_ui("Tables_1")),
            mainPanel(
              mod_Show_tables_ui("Show_tables_1"))
            ),
          # Volet Statistiques exploratoires ####
          tabItem(tabName = "exploratory"),
          #Onglet plot indicator
          tabItem(tabName = "Indic",
                  mod_Indicator_ui("Indicator_1")
          ),
          #Onglet plot structure
          tabItem(
            tabName = "Struc",
            mod_Structure_ui("Structure_1")
          ),
          # Volet Statistiques descriptives ####
          tabItem(tabName = "descriptive"),
          #Onglet plot
          tabItem(tabName = "plot",
                  mod_Stat_ui("Stat_1")
          ),
          #Onglet choix de modelisation
          tabItem(
            tabName = "Diag",
            sidebarPanel(
              mod_Choix_loi_ui("Choix_loi_1")),
            mainPanel(
              mod_Analyse_choice_ui("Analyse_choice_1")),
          ),
          # Volet GLMMs ####
          tabItem(tabName = "glmm"),
          # onglet modélisation
          tabItem(
            tabName = "mod",
            sidebarPanel(
              mod_Prepare_modelling_ui("Prepare_modelling_1")
            ),
            mainPanel(
              mod_Modelling_ui("Modelling_1"),
            )),
          #Onglet représentation
          tabItem(
            tabName = "rep",
            mod_Representation_ui("Representation_1")),
          #onglet puissance
          tabItem(
            tabName = "pui",
            mod_Puissance_ui("Puissance_1"))
     ))
    )
  )
)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "myGranulat"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
