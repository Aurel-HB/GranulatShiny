#' This script demonstrates how to use shiny.i18n Translator object
#' for live language change on the UI side with Shiny modules. Two key steps are:
#' (a) add `usei18n(i18n)` to UI
#' (b) use `update_lang` function to change the language in session

#library(shiny)
#library(shiny.i18n)

# File with translations
#i18n <- Translator$new(translation_csvs_path = "../data/")
#i18n$set_translation_language("fr") # here you select the default translation to display

mod_lang_ui <- function(id) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
      selectInput(ns("selected_language"),
        i18n$t("Langues"),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      ))
}

mod_lang_server<- function(id) {
  moduleServer(id, function(input, output, session) {
    # ObserveEvent to listen the select input values
    observeEvent(input$selected_language, {
      # Here is where we update language in session
      update_lang(input$selected_language)
    })
  })
}

#ui <- fluidPage(
#  mod_lang_ui("app")
#)

#server <- function(input, output, session) {
#  mod_lang_server("app")
#}

#shinyApp(ui, server)
