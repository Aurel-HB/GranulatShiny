#' Indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Indicator_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      dataTableOutput(ns("diversity")),
      downloadButton(ns("downloadData"), label = "Telecharger le tableau (.csv)"),
      width = NULL,
      style = "overflow-x: scroll;",
      collapsible = T,
      solidHeader = TRUE,
      status = "primary"
    ),
    #verbatimTextOutput(ns("test")),
    box(
      uiOutput(ns("choix_campagne")),
      plotOutput(ns("lineplots")),
      #telecharger le graphique
      downloadButton(ns("downloadPlot"),
                     label = "Telecharger le graphique (.png)"),
      collapsible = T,
      collapsed = T,
      solidHeader = TRUE,
      status = "info",
      width = NULL,
      title = "Représentation des indicateurs"
      ),
    hr(),
    actionButton("struct", "Structure de la communeauté")
  )
}

#' Indicator Server Functions
#'
#' @noRd
mod_Indicator_server <- function (input, output, session, r){
  ns <- session$ns
  dataset <- reactive({
    if(is.null(r$data_forme)){return()}
    as.data.frame(r$data_forme[[1]])
    })
  #output$test <- renderPrint({class(input$choix_campagne)})

  ID_campagne <- reactive({
    if(is.null(dataset())){return()}
    ID_camp <- c()
    for (i in (1:max(dataset()['campagne']))){
      ID_camp <- c(ID_camp, paste("C",i,sep=""))
    }
    return(ID_camp)
  })

  observe({
    r$ID_campagne <- ID_campagne()
  })

  data_indic <- reactive ({
    if(is.null(r$data_forme)){return()}
    cbind(
      "ID_campagne" = ID_campagne(),
    diversity_table(dataset(), "Abun"),
    diversity_table(dataset(), "Biom" ),
    diversity_table(dataset(), "Richness" ),
    diversity_table(dataset(), "Shannon" ),
    diversity_table(dataset(), "Simpson" )
    )
    })


  show_data_indic <- reactive({
    if(is.null(r$data_forme)){return()}

    name_indic <- names(data_indic())
    position <- grep("value", name_indic)
    data <- data_indic()[,position]
    data <- data.frame("ID_campagne" = data_indic()[,1], data)
    return(data)
  })

  output$diversity <- renderDataTable({show_data_indic()},
                                  options = list("pageLength" = 10))

  # Donwload data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_indic", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(show_data_indic(),  file)
    }
  )


  #### boxplot part ####
  output$choix_campagne <- renderUI({
    selectInput(
      ns("choix_campagne"),
      "Sélectionner la campagne :",
      ID_campagne(),
      selected = ID_campagne()[1]
    )
  })

  campagne <- reactive({
    if (is.null(input$choix_campagne)){return()}
    input$choix_campagne
  })

  line_plots <- reactive({
    if (is.null(data_indic())){return()}
    if (is.null(input$choix_campagne)){return()}

    abun_plot <- lineplot_creation(data_indic(), "Abun", campagne())

    biom_plot <- lineplot_creation(data_indic(), "Biom", campagne())
    richness_plot <- lineplot_creation(data_indic(), "Richness", campagne())
    shannon_plot <- lineplot_creation(data_indic(), "Shannon", campagne())
    simpson_plot <- lineplot_creation(data_indic(), "Simpson", campagne())

    frise <- plot_grid(abun_plot, biom_plot, richness_plot,
                       shannon_plot, simpson_plot)
    return(frise)
  })


  output$lineplots <- renderPlot({
    line_plots()
  })

  ## Exporter le graphique
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot_indic_", input$choix_campagne, ".png", sep = "")
    },
    content = function(file) {
      # Use tryCatch to handle errors with try(silent = TRUE)
      tryCatch(
        {
          ggsave(file, plot = line_plots(), height = 9, width = 16, bg = "white")
        },
        error = function(e) {
          # Handle the error here (print a message, log it, etc.)
          print("")
        },
        warning = function(w) {
          # Handle warnings if needed
          print("")
        }
      )
    })

}

## To be copied in the UI
# mod_Indicator_ui("Indicator_1")

## To be copied in the server
# mod_Indicator_server("Indicator_1")
