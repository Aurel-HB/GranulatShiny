#' simple_02 UI Function
#'
#' @description Le module utilisé quand on ne prend pas en compte la stratége d'extraction.
#' Juste le module traitement, developpement independant du module complexe  
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simple_02_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel("output.operation_charge",
                     tabBox(#title = "Information sur l'extraction",
                       id=ns("tab"),
                       tabPanel("Stations d'impact", traitementUI(ns("zone1"))))
                     
                     
                     
                     
    )
  )
}


#' simple_02 Server Functions
#'
#' @noRd 
mod_simple_02_server <- function(id, operation, r, test){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Module pour la variables traitement
    
    ls1 <- traitementServer("zone1", operation, "Période 1", test)
    
    
    ls_station <-
      reactive({
        list(ls1()[[1]])
      })
    ls_dates <-
      reactive({
        list(ls1()[[2]])
      })
    
    
    
    liste<-reactive({list(
      ls_station(),
      ls_dates()
    )})
    observe({
      r$module<-liste()
    })
    return(r)
    
    
  })
}

## To be copied in the UI
# mod_simple_02_ui("simple_02_ui_1")

## To be copied in the server
# mod_simple_02_server("simple_02_ui_1")
