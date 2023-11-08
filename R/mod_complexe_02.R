#' complexe_02 UI Function
#'
#' @description Le module utilisé quand on prend en compte la stratége d'extraction.
#' Le module traitement multiplié par le nombre de sous zone
#' A adapter selon la méthodologie, developpement independant du module simple
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_complexe_02_ui <- function(id){
  ns <- NS(id)
  tagList(
    conditionalPanel("output.operation_charge",
                     tabBox(#title = "Information sur l'extraction",
                       id=ns("tab"),
                       tabPanel("Sous zone 1", traitementUI(ns("zone1"))),
                       tabPanel("Sous zone 2", traitementUI(ns("zone2"))),
                       tabPanel("Sous zone 3", traitementUI(ns("zone3"))),
                       tabPanel("Sous zone 4", traitementUI(ns("zone4"))),
                       tabPanel("Sous zone 5", traitementUI(ns("zone5"))),
                       tabPanel("Sous zone 6", traitementUI(ns("zone6"))),
                       tabPanel("Sous zone 7", traitementUI(ns("zone7"))),
                       tabPanel("Sous zone 8", traitementUI(ns("zone8"))))


    )

  )
}

#' complexe_02 Server Functions
#'
#' @noRd
mod_complexe_02_server <- function(id, operation, r, test){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # Modules pour la variables traitement selon le nombre de zones (rep le module 8 fois pour max 8 zones)
    ls1 <- traitementServer("zone1", operation, "Sous zone 1", test)
    ls2 <- traitementServer("zone2", operation, "Sous zone 2", test)
    ls3 <- traitementServer("zone3", operation, "Sous zone 3", test)
    ls4 <- traitementServer("zone4", operation, "Sous zone 4", test)
    ls5 <- traitementServer("zone5", operation, "Sous zone 5", test)
    ls6 <- traitementServer("zone6", operation, "Sous zone 6", test)
    ls7 <- traitementServer("zone7", operation, "Sous zone 7", test)
    ls8 <- traitementServer("zone8", operation, "Sous zone 8", test)

    ls_station <-
      reactive({
        list(ls1()[[1]],
             ls2()[[1]],
             ls3()[[1]],
             ls4()[[1]],
             ls5()[[1]],
             ls6()[[1]],
             ls7()[[1]],
             ls8()[[1]])
      })
    ls_dates <-
      reactive({
        list(ls1()[[2]],
             ls2()[[2]],
             ls3()[[2]],
             ls4()[[2]],
             ls5()[[2]],
             ls6()[[2]],
             ls7()[[2]],
             ls8()[[2]])
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
# mod_complexe_02_ui("complexe_02_ui_1")

## To be copied in the server
# mod_complexe_02_server("complexe_02_ui_1")
