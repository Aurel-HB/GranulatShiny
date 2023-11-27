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
      tableOutput(ns("var_summary")),
      box( title = "Histogram",
           status = "success", #  Valid statuses are: primary, success, info, warning, danger.
           solidHeader = TRUE,
        plotOutput(ns("hist")),
        actionButton("goloi", "Passer au choix de la loi de distribution")
      ),
      box( title = "Interactionplot",
           solidHeader = TRUE,
           status = "info",
           collapsible = TRUE,
           collapsed = TRUE,
           plotOutput(ns("interact"))
      ),
      box( title = "Boxplot",
           solidHeader = TRUE,
           status = "info",
        collapsible = TRUE,
        collapsed = TRUE,
        plotOutput(ns("box_impact")),
        plotOutput(ns("box_saison"))
      )
      #box(title = "Coplot",
      #    solidHeader = T,
      #    status = "info",
      #    collapsible = T,
      #    plotOutput(ns("co_saison_traitement"))),
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
      if(is.null(data_analyse())){return()}
      numeric_summary(as.numeric(data_analyse()[,1]), names(data_analyse())[1])
    })

    output$hist <- renderPlot({
      if(is.null(data_analyse())){return()}
      hist(as.numeric(data_analyse()[,1]),
           main = paste("Histogram of ", var_name(), sep=""),
           xlab = r$var_name, ylab = "Frequency", col = "lightblue")
    })

    output$box_impact <- renderPlot({
      if(is.null(data_analyse())){return()}
      boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$traitement,
              data = data_analyse(),
              main = paste("Boxplot of ", var_name()," by impact", sep=""),
              xlab = "", ylab = ""
              )
    })

    output$box_saison <- renderPlot({
      if(is.null(data_analyse())){return()}
      boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison,
              data = data_analyse(),
              main = paste("Boxplot of ", var_name()," by season", sep=""),
              xlab = "", ylab = ""
              )
    })

    #output$co_saison_traitement <- renderPlot({
    #  if(is.null(data_analyse())){return()}
    #  coplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison | data_analyse()$traitement,
    #         data = data_analyse(),
    #         main = paste("Coplot of ", var_name()," by season and traitment", sep=""),
    #         xlab = "", ylab = "", xlim = (c(min(as.numeric(data_analyse()[,1])),
    #                                         max(as.numeric(data_analyse()[,1]))))
    #         )
    #})

    output$interact <- renderPlot({
      if(is.null(data_analyse())){return()}
      interaction.plot(x.factor=data_analyse()$saison,
                       trace.factor=data_analyse()$traitement,
                       trace.label = "traitment",
                       response=as.numeric(data_analyse()[,1]),
                       main = paste("Interaction_plot of ", var_name()," by season and traitment", sep=""),
                       xlab = "", ylab = "")
    })

}

## To be copied in the UI
# mod_Stat_ui("Stat_1")

## To be copied in the server
# mod_Stat_server("Stat_1")
