#' Modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Modelling_ui <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}

#' Modelling Server Functions
#'
#' @noRd
mod_Modelling_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Modelling_ui("Modelling_1")

## To be copied in the server
# mod_Modelling_server("Modelling_1")
