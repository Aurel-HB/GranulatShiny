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
  dataset <- reactive({as.data.frame(r$data_forme[[1]])})
  #output$test <- renderPrint({class(input$choix_campagne)})

  ID_campagne <- reactive({
    ID_camp <- c()
    for (i in (1:max(dataset()['campagne']))){
      ID_camp <- c(ID_camp, paste("C",i,sep=""))
    }
    return(ID_camp)
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
}

## To be copied in the UI
# mod_Indicator_ui("Indicator_1")

## To be copied in the server
# mod_Indicator_server("Indicator_1")
