#' Stat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Stat_ui <- function(id){
  ns <- NS(id)
  tagList(
    #conditionalPanel(
      #"input.stat != false",
      tableOutput(ns("var_summary")),
      box( title = "Histogram",
           solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(ns("hist"), height = 500)
      ),
      box( title = "Boxplot",
           solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(ns("box_impact")),
        plotOutput(ns("box_saison"))
      )#,
      #plotOutput(ns("hist")),
      #plotOutput(ns("box_impact")),
      #plotOutput(ns("box_saison"))
    #)
  )
}

#' Stat Server Functions
#'
#' @noRd
mod_Stat_server <- function(input, output, session, r){
    ns <- session$ns

    data_analyse <- reactive({
      r$data_analyse
    })

    var_name <- reactive({
      r$var_name
    })

    output$var_summary <- renderTable({
      numeric_summary(as.numeric(data_analyse()[,1]), names(data_analyse())[1])
    })

    output$hist <- renderPlot({
      hist(as.numeric(data_analyse()[,1]),
           main = paste("Histogram of ", var_name(), sep=""),
           xlab = r$var_name, ylab = "Frequency", col = "lightblue")
    })

    output$box_impact <- renderPlot({
      boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$traitement,
              data = data_analyse(),
              main = paste("Boxplot of ", var_name()," by impact", sep=""),
              xlab = "", ylab = ""
              )
    })

    output$box_saison <- renderPlot({
      boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison,
              data = data_analyse(),
              main = paste("Boxplot of ", var_name()," by season", sep=""),
              xlab = "", ylab = ""
              )
    })


}

## To be copied in the UI
# mod_Stat_ui("Stat_1")

## To be copied in the server
# mod_Stat_server("Stat_1")
