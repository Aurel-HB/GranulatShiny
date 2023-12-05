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
      box( title = "Summary",
           status = "success", #  Valid statuses are: primary, success, info, warning, danger.
           solidHeader = TRUE,
           box(
             tableOutput(ns("var_summary")),
             width = NULL,
             style = "overflow-x: scroll;",
             collapsible = F,
             solidHeader = F
           ),
        #tableOutput(ns("var_summary")),
        plotOutput(ns("hist")),
        actionButton("goloi", "Passer au choix de la loi de distribution")
      ),
      box( title = "Interactionplot",
           solidHeader = TRUE,
           status = "info",
           collapsible = TRUE,
           collapsed = TRUE,
           style = "overflow-x: scroll;",
           uiOutput(ns("choix_interaction")),
           plotOutput(ns("interact"))
      ),
      box( title = "Boxplot",
           solidHeader = TRUE,
           status = "info",
        collapsible = TRUE,
        collapsed = TRUE,
        uiOutput(ns("choix_box")),
        plotOutput(ns("boxplot"))
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

    #### boxplot part ####
    output$choix_box <- renderUI({
      selectInput(
        ns("choix_box"),
        "Sélectionner la covariable :",
        c("impact" = "1",
          "year" = "2",
          "campagne" = "3",
          "station" = "4",
          "saison" = "5"),
        selected = "1"
      )
    })

    output$boxplot <- renderPlot({
      if(is.null(data_analyse())){return()}
      if(is.null(input$choix_box)){return()}
      if(input$choix_box == 1){
        boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$traitement,
                data = data_analyse(),
                main = paste("Boxplot of ", var_name()," by impact", sep=""),
                xlab = "", ylab = ""
        )
      }
      if(input$choix_box == 2){
        boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$year,
                data = data_analyse(),
                main = paste("Boxplot of ", var_name()," by year", sep=""),
                xlab = "", ylab = ""
        )
      }
      if(input$choix_box == 3){
        boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$campagne,
                data = data_analyse(),
                main = paste("Boxplot of ", var_name()," by survey", sep=""),
                xlab = "", ylab = ""
        )
      }
      if(input$choix_box == 4){
        boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$station,
                data = data_analyse(),
                main = paste("Boxplot of ", var_name()," by station", sep=""),
                xlab = "", ylab = ""
        )
      }
      if(input$choix_box == 5){
        boxplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison,
                data = data_analyse(),
                main = paste("Boxplot of ", var_name()," by season", sep=""),
                xlab = "", ylab = ""
        )
      }
    })


    #output$co_saison_traitement <- renderPlot({
    #  if(is.null(data_analyse())){return()}
    #  coplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison | data_analyse()$traitement,
    #         data = data_analyse(),
    #         main = paste("Coplot of ", var_name()," by season and traitment", sep=""),
    #         xlab = "", ylab = "", xlim = (c(min(as.numeric(data_analyse()[,1])),
    #                                         max(as.numeric(data_analyse()[,1]))))
    #         )
    #}) pas lisible

    #### interaction plot part ####
    output$choix_interaction <- renderUI({
      selectInput(
        ns("choix_interaction"),
        "Sélectionner l'interaction :",
        c("traitement_saison" = "1",
          "traitement_year" = "2",
          "traitement_campagne" = "3",
          "traitement_station" = "4"),
        selected = "1"
      )
    })

    output$interact <- renderPlot({
      if(is.null(data_analyse())){return()}
      if(is.null(input$choix_interaction)){return()}
      if(input$choix_interaction == 1){
        interaction.plot(x.factor=data_analyse()$saison,
                         trace.factor=data_analyse()$traitement,
                         trace.label = "traitment",
                         response=as.numeric(data_analyse()[,1]),
                         main = paste("Interaction_plot of ", var_name()," by season and traitment", sep=""),
                         xlab = "", ylab = "")
      }
      if(input$choix_interaction == 2){
        interaction.plot(x.factor=data_analyse()$year,
                         trace.factor=data_analyse()$traitement,
                         trace.label = "traitment",
                         response=as.numeric(data_analyse()[,1]),
                         main = paste("Interaction_plot of ", var_name()," by year and traitment", sep=""),
                         xlab = "", ylab = "")
      }
      if(input$choix_interaction == 3){
        interaction.plot(x.factor=data_analyse()$campagne,
                         trace.factor=data_analyse()$traitement,
                         trace.label = "traitment",
                         response=as.numeric(data_analyse()[,1]),
                         main = paste("Interaction_plot of ", var_name()," by survey and traitment", sep=""),
                         xlab = "", ylab = "")
      }
      if(input$choix_interaction == 4){
        interaction.plot(x.factor=data_analyse()$station,
                         trace.factor=data_analyse()$traitement,
                         trace.label = "traitment",
                         response=as.numeric(data_analyse()[,1]),
                         main = paste("Interaction_plot of ", var_name()," by sation and traitment", sep=""),
                         xlab = "", ylab = "")
      }
    })

}

## To be copied in the UI
# mod_Stat_ui("Stat_1")

## To be copied in the server
# mod_Stat_server("Stat_1")
