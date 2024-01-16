#' Structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Structure_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("En cours de dèveloppement"),
    #verbatimTextOutput(ns("test")),
    tableOutput(ns("percent")),
    actionButton("descript", "Stat descriptives")
  )
}

#' Structure Server Functions
#'
#' @noRd
mod_Structure_server <- function (input, output, session, r){
    ns <- session$ns
    dataset <- reactive({
      if(is.null(r$data_forme)){return()}
      as.data.frame(r$data_forme[[1]])
      })
    species <- reactive({r$species[,1]})
    #"year""station""date""saison""campagne""tow""hauled_surf""traitement""interaction""Abun""Biom""Richness""Shannon""Simpson""Pielou"


    data_brut <- reactive ({
      if(is.null(r$data_forme)){return()}
      if(is.null(r$ID_campagne)){return()}
      data <- data.frame(r$ID_campagne)
      for (sp in species()){
        data <- cbind(data, structure_table(dataset(), sp))
      }
      position <- grep("tot_value", names(data))
      data <- data[,c(position)] #as.data.frame(data[,c(1,5,8)])
      tot <- rowSums(data)
      data <- cbind(r$ID_campagne, data, tot)
      names(data) <- c("ID_campagne",species(),"Total")
      data
    })

    data_percent <- reactive({
      if(is.null(data_brut())){return()}
      data <- data_brut()
      for (i in 1:nrow(data)){
        for (sp in 1:length(species())){
          data[i,sp+1] <- round(data[i,sp+1]/data$Total[i], digits = 2)
        }
      }
      data_t <- t(data[,2:(length(species())+1)])
      #redefine row and column names
      rownames(dtat_t) <- species()
      colnames(data_t) <- r$ID_campagne
      data_t <- cbind(species(),data_t)
      data_t
    })

    output$percent <- renderTable({
      data_percent()
    })

}

## To be copied in the UI
# mod_Structure_ui("Structure_1")

## To be copied in the server
# mod_Structure_server("Structure_1")
