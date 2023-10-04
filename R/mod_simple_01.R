#' simple_01 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simple_01_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' simple_01 Server Functions
#'
#' @noRd 
mod_simple_01_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_simple_01_ui("simple_01_ui_1")
    
## To be copied in the server
# mod_simple_01_server("simple_01_ui_1")
