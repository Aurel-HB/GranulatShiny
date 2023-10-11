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
#'
#'

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
          "Mise en forme des données",
          tabName = "forme",
          icon = icon("readme"),
          menuSubItem("Informations à rentrer", tabName = "donnees"),
          menuSubItem("Tables", tabName = "table")
        ),
        menuItem(
          "Statistiques descriptives",
          tabName = "descriptive",
          icon = icon("far fa-chart-bar")
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
        tabItems(
          #onglet Information à rentrer
          tabItem(
            tabName = "donnees",
            
            sidebarPanel(
              selectInput(
                "complex",
                "Prendre en compte la stratégie d'extraction ?",
                c("non" = "1", "oui" = "2")
              ),
              fileInput("tutti_catch", "Sélectionnez le fichier Tutti Catch"),
              fileInput("tutti_operation",
                        "Sélectionnez le fichier Tutti operation"),
              conditionalPanel(
                "output.operation_charge",
                fileInput(
                  "shpFile",
                  "Sélectionnez les Shapefiles (.shp, .shx, .dbf)",
                  multiple = T
                ),
                fileInput("uploadSave", "Charger les informations à rentrer")
              ),
              conditionalPanel(
                'input.complex==2',
                numericInput(
                  "zones",
                  "Nombre de sous-zone d'extraction ?",
                  min = 1,
                  max = 8,
                  value = 1
                )
              ),
              hr(),
              actionButton("go", "Mettre en forme")
            ),
            mainPanel(
              verbatimTextOutput("testprint"),
              
              conditionalPanel('input.complex==1', mod_simple_02_ui("simple_02_ui_1")),
              conditionalPanel(
                'input.complex==2',
                mod_complexe_02_ui("complexe_02_ui_1")
              ),
              conditionalPanel(
                "output.operation_charge",
                tabBox(id = "tab_reference",
                       tabPanel(
                         "Stations de référence", uiOutput("reference")
                       )),
                leafletOutput("carte")
              )
            )
            
            
            
          ),
          #Onglet Tables
          tabItem(
            tabName = "table",
            sidebarPanel(
              selectInput(
                "list",
                "Quelle table afficher ?",
                c(
                  "Table complete" = "1",
                  "Abondance" = "2",
                  "Biomasse" = "3",
                  "Richesse" = "4",
                  "Simpson" = "5"
                ),
                selected = "Table complete"
              ),
              downloadButton("downloadData", label = "Telecharger la table"),
              hr(),
              downloadButton("downloadSave", label = "Telecharger les informations rentrées")
            ),
            mainPanel(conditionalPanel(
              "input.go != false",
              box(
                dataTableOutput("contents"),
                width = NULL,
                style = "overflow-x: scroll;",
                collapsible = T,
                solidHeader = TRUE
              )
            ))
          ),
          # Volet Statistiques descriptives
          tabItem(tabName = "descriptive", h1("En développement")),
          # Volet GLMMs
          tabItem(tabName = "glmm"),
          # onglet modélisation
          tabItem(
            tabName = "mod",
            #Input des glmms
            sidebarPanel(
              uiOutput("variable_y"),
              selectInput(
                "methode",
                "Sélectionnez la méthode statistique",
                c(
                  "GLMM" = "1",
                  "GLM" = "2",
                  "Permanova" = "3"
                )
              ),
              conditionalPanel(
                "input.methode != 3",
                selectInput(
                  "loi",
                  "Sélectionnz la loi de distribution",
                  c(
                    "Normale",
                    "Binomiale" = "binomial",
                    "Poisson" = "poisson",
                    "Binomiale négative",
                    "Gamma log",
                    "Gamma inverse"
                  )
                )
              ),
              checkboxInput("interaction", "Voulez-vous retirer l'interaction ?"),
              uiOutput("covariable"),
              
              hr(),
              h4(strong("Formulation du modèle :")),
              htmlOutput("ecriture_modele"),
              htmlOutput("ecriture_loi"),
              hr(),
              actionButton("go2", "Lancer la modélisation"),
              uiOutput("choix_modele"),
              conditionalPanel(
                "input.go2 != false",
                selectInput(
                  "choix_sortie",
                  "Afficher les sorties :",
                  c(
                    "Anova" = "1",
                    "Summary" = "2",
                    "Vérification" = "3"
                  ),
                  selected = "1"
                )
              )
              
              
              
              
            ),
            # Output des glmms
            mainPanel(
              # l'histogramme
              plotOutput("plot_y"),
              #formulation du modèle
              verbatimTextOutput("modele"),
              #plot de vérification
              conditionalPanel("input.choix_sortie == 3", plotOutput("verification"))
            )
            
          ),
          #Onglet représentation
          tabItem(
            tabName = "rep",
            # Inputs pour parametrer le graph
            sidebarPanel(
              #choix du terme
              uiOutput("effet_choix"),
              #afficher ou non les lettres post-hocs
              checkboxInput("post_hoc", "Afficher les lettres de significativitées"),
              # gerer la place des lettres
              conditionalPanel(
                "input.post_hoc == true",
                numericInput(
                  "largeur_plot",
                  "Régler la largeur des lettres",
                  value = NULL,
                  step = 0.1
                )
              ),
              conditionalPanel(
                "input.post_hoc == true",
                numericInput(
                  "hauteur_plot",
                  "Régler la hauteur des lettres",
                  value = NULL,
                  step = 0.1
                )
              ),
              #telecharger le graphique
              downloadButton("downloadPlot_effet", label = "Telecharger le graphique")
            ),
            #Output du grah
            mainPanel(plotOutput("effet_plot") %>% withSpinner(color =
                                                                 "#00619a"))
          ),
          #Onglet puissance
          tabItem(
            tabName = "pui",
            # Inputs pour parametrer le test de puissance
            sidebarPanel(
              #choix du terme
              uiOutput("puissance_choix"),
              #nombre de campagne à simuler
              numericInput(
                "nbr_campagne",
                "Combien de campagne supplémentaire à simuler ?",
                value = 2,
                min = 1,
                step = 1
              ),
              actionButton("go3", "GO")
              
            ),
            #Output du grah
            mainPanel(
              plotOutput("plot_puissance") %>% withSpinner(color = "#00619a")
              
            )
          )
          
        )
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
  golem::add_resource_path('www', app_sys('app/www'))
  
  tags$head(
    favicon("GranulatShiny"),
    bundle_resources(path = app_sys('app/www'),
                     app_title = 'GranulatShiny')
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
