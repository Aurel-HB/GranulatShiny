#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyBS
#' @import shinydashboard
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import shinycssloaders
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( # create the left part of the interface to choose the page
      skin = "blue",
      dashboardHeader(title = "GranulatShiny"),
      dashboardSidebar(sidebarMenu(
        id = "tabs",
        menuItem(
          "Page d'accueil",
          tabName = "accueil",
          icon = icon("far fa-file-lines")
        ),
        menuItem(
          "Mise en forme des données",
          tabName = "forme",
          icon = icon("readme"),
          menuSubItem("Informations à rentrer", tabName = "donnees"),
          menuSubItem("Tables", tabName = "table")
        ),
        menuItem(
          "Statistiques descriptives",
          tabName = "descriptive",
          icon = icon("far fa-chart-bar"),
          menuSubItem("Plot des données", tabName = "plot"),
          menuSubItem("Diagnostique d'analyse", tabName = "Diag")
        ),
        menuItem(
          "Modélisation",
          tabName = "glmm",
          icon = icon("fish"),
          menuSubItem("Création des modèles", tabName = "mod"),
          menuSubItem("Représentation des effets", tabName = "rep"),
          menuSubItem("Puissance statistique", tabName = "pui")

        )
      )),
      dashboardBody( # create the interface for each tab
        fluidPage(
        tabItems(
          #page d'accueil
          tabItem(
            tabName = "accueil",
            mod_reception_ui("reception_1")
          ),
          #onglet Information à rentrer
          tabItem(
            tabName = "donnees",
            sidebarPanel(mod_Import_data_ui("Import_data_1"),
                         mod_Prepare_data_ui("Prepare_data_1")#,
                         #actionButton("go", "Mettre en forme")
                         )  ,
            mainPanel(
              tabBox(id = "tab",
                tabPanel("Stations d'impact", mod_Impact_ui("Impact_1")
                         )),
              tabBox(id = "tab_reference",
                     tabPanel(
                       "Stations de référence", mod_Ref_ui("Ref_1")
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
          # Volet Statistiques descriptives
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
          # Volet GLMMs
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
