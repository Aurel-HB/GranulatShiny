#' traitement UI Function
#'
#' @description Module qui permet d'afficher les inputs necessaires à la création de la variabe traitement :
#' les stations d'impact et les dates associées. le module retourne une liste de ces infos.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
traitementUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("datesOut")),
    uiOutput(ns("stationOut")))
  
}
    
#' traitement Server Functions
#'
#' @noRd 
traitementServer <- function(id, tutti_operation, label, test) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 test
                 output$numero <- renderText({
                   label
                 })
                 output$datesOut <-
                   renderUI({
                     dateRangeInput(
                       ns("dates"),
                       "Dates de la période d'activité :",
                       start = min(as.Date(
                         tutti_operation$DateDeb,  "%d/%m/%Y"
                       )),
                       end = max(as.Date(
                         tutti_operation$DateDeb,  "%d/%m/%Y"
                       )),
                       format = "yyyy-mm-dd",
                       startview = "month",
                       weekstart = 0,
                       language = "fr",
                       separator = " jusqu'au "
                     )
                   })
                 output$stationOut <- renderUI({
                   selectInput(
                     ns("station"),
                     "Stations à l'intérieur de la zone d'étude :",
                     multiple = T,
                     choices = levels(as.factor(tutti_operation$Code_Station))
                   )
                 })
                 
                 observeEvent(test, {
                   updateSelectInput(inputId =  "station", selected = test[[1]][[1]])
                   updateDateRangeInput(inputId = "dates", start = test[[2]][[1]], end = test[[3]][[1]] )
                 })
                 reactive({
                   list(
                   input$station,
                   input$dates
                 )
                 })
                 
               })
}
    
## To be copied in the UI
# mod_traitement_ui("traitement_ui_1")
    
## To be copied in the server
# mod_traitement_server("traitement_ui_1")
