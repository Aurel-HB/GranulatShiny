#' simple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simple_ui <- function(id){
  ns <- NS(id)
  tagList(
    
sidebarPanel(
      fileInput(ns("tutti_catch"), "Sélectionnez le fichier Tutti Catch"),
      fileInput(
        ns("tutti_operation"),
        "Sélectionnez le fichier Tutti operation"
      ),

      conditionalPanel("output.operation_charge", ns=ns, fileInput(ns("shpFile"), "Sélectionnez les Shapefiles (.shp, .shx, .dbf)", multiple = T)),
      hr(),
      actionButton(ns("go"), "Mettre en forme"),
      ),
    mainPanel(
      conditionalPanel("output.operation_charge", ns=ns,
                       tabBox(#title = "Information sur l'extraction",
                         id=ns("tab"),
                         tabPanel("Stations d'impact", traitementUI(ns("zone1")))),
                       tabBox(id=ns("tab_reference"), 
                              tabPanel("Stations de référence", uiOutput(ns("reference")))),
                       leafletOutput(ns("carte"))
                       
      ))
  )
}

    
#' simple Server Functions
#'
#' @noRd 
mod_simple_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  # Charger les données
    tutti_catch <- reactive({
      file1 <- input$tutti_catch[, 4]
      data11 <- read.csv (file1, header = T, sep = ";")
    })
    
    tutti_operation <- reactive({
      file2 <- input$tutti_operation[, 4]
      data22 <- read.csv (file2, header = T, sep = ";")
    })
    # pour faire marcher conditionalpanel dans le UI
    
    output$operation_charge <- reactive({
      return(!is.null(input$tutti_operation))
    })
    
    outputOptions(output, "operation_charge", suspendWhenHidden = FALSE)
    
    # Shape file
    shape <- reactive({
      shape_file <- input$shape$datapath[1]
      shape <- st_read(shape_file)
      polygon <- shape[["geometry"]][[1]] ## marche pas pour  resiste = [[23]][[2]], possiblement rajouter un input pour le choix de la ligne pour les shapefiles avec plusieurs lignes
    })
    
    shape <- reactive({
      if (!is.null(input$shpFile)) {
        shpDF <- input$shpFile
        prevWD <- getwd()
        uploadDirectory <- dirname(shpDF$datapath[1])
        setwd(uploadDirectory)
        for (i in 1:nrow(shpDF)) {
          file.rename(shpDF$datapath[i], shpDF$name[i])
        }
        shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
        shpPath <- paste(uploadDirectory, shpName, sep = "/")
        setwd(prevWD)
        shpFile <- st_read(shpPath)
        polygon <- shpFile[["geometry"]][[1]] # [[1]] à changer pour RESISTE
        return(polygon)
      } else {
        return()
      }
    })

 
    # Modules pour la variables traitement

    ls1 <- traitementServer("zone1", tutti_operation(), "Période 1")

    
    ls_station <-
      reactive({
        list(ls1()[[1]])
      })
    ls_dates <-
      reactive({
        list(ls1()[[2]])
      })
    
    # Représentation des stations
    tutti_operation_bis <-
      reactive({
        pre_leaflet(tutti_operation(), ls_station(), ls_dates(), 1)
      })
    output$reference <- renderUI({
      checkboxGroupInput(
        ns("reference"),
        label = "Si besoin, séléctionner les stations à retirer :",
        choices = unique(tutti_operation_bis()[which(is.na(tutti_operation_bis()$impact_date_1)),]$Code_Station),
        inline = T
      )
    })
    station_ban <- reactive({
      input$reference
    })
    
    # filtrer les stations à retirer
    tutti_operation_filtre <- reactive({
      if (is.null(station_ban())) {
        tutti_operation_bis()
      } else {
        tutti_operation_bis()[-which(tutti_operation_bis()$Code_Station %in% station_ban()),]
      }
    })
    
    tutti_catch_filtre <- reactive({
      if (is.null(station_ban())) {
        tutti_catch()
      } else {
        tutti_catch()[-which(tutti_catch()$Trait %in% station_ban()),]
      }
    })
    
    #leaflet
    output$carte <- renderLeaflet({
      carte <- leaflet_maker(tutti_operation_filtre(), shape())
    })
    
    # Mettre en forme et calcul des indicateurs
    data_forme <- eventReactive(input$go, {
      global_function(
        tutti_catch_filtre(),
        tutti_operation_filtre(),
        ls_station(),
        ls_dates(),
        1
      )
    })
    
    list(
      input$go,
      data_forme())
    
    
  })
}
    
## To be copied in the UI
# mod_simple_ui("simple_ui_1")
    
## To be copied in the server
# mod_simple_server("simple_ui_1")
