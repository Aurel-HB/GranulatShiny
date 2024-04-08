#' Puissance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Puissance_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    # Inputs pour parametrer le test de puissance
    #sidebarPanel(
      h1(i18n$t("En cours de développement")),
      sidebarPanel(
        textOutput(ns("explain"))
      )
    #),
    #Output du grah
    #mainPanel(
      #plotOutput(ns("plot_puissance")) %>% withSpinner(color = "#006522")

    #)
  )
}

#' Puissance Server Functions
#'
#' @noRd
mod_Puissance_server <- function(input, output, session, r){
    ns <- session$ns

    text_info <- reactive({
      as.character(list_translate[r$lang][13,1])
    })

    output$explain <- renderText({
      text_info()
    })
}

## To be copied in the UI
# mod_Puissance_ui("Puissance_1")

## To be copied in the server
# mod_Puissance_server("Puissance_1")
