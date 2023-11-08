#' Representation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Representation_ui <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}

#' Representation Server Functions
#'
#' @noRd
mod_Representation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Representation_ui("Representation_1")

## To be copied in the server
# mod_Representation_server("Representation_1")
