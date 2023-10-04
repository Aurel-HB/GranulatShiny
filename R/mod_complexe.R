#' complexe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_complexe_ui <- function(id){
  ns <- NS(id)
  tagList(
                sidebarPanel(
                  fileInput(ns("tutti_catch"), "Sélectionnez le fichier Tutti Catch")
                  ,
                  fileInput(
                    ns("tutti_operation"),
                    "Sélectionnez le fichier Tutti operation"
                  ),
                  numericInput(
                    ns("zones"),
                    "Nombre de sous zone d'extraction ?",
                    min = 1,
                    max = 8,
                    value = NULL
                  ),
                  
                  conditionalPanel("input.zones != null", ns=ns, fileInput(ns("shpFile"), "Sélectionnez les Shapefiles (.shp, .shx, .dbf)", multiple = T)),
                  hr(),
                  actionButton(ns("go"), "Mettre en forme")
                ),
                mainPanel(
                  conditionalPanel("input.zones != null", ns=ns,
                                   tabBox(#title = "Information sur l'extraction",
                                     id=ns("tab"),
                                     tabPanel("Sous zone 1", traitementUI(ns("zone1"))),
                                     tabPanel("Sous zone 2", traitementUI(ns("zone2"))),
                                     tabPanel("Sous zone 3", traitementUI(ns("zone3"))),
                                     tabPanel("Sous zone 4", traitementUI(ns("zone4"))),
                                     tabPanel("Sous zone 5", traitementUI(ns("zone5"))),
                                     tabPanel("Sous zone 6", traitementUI(ns("zone6"))),
                                     tabPanel("Sous zone 7", traitementUI(ns("zone7"))),
                                     tabPanel("Sous zone 8", traitementUI(ns("zone8")))),
                                   tabBox(id=ns("tab_reference"), 
                                          tabPanel("Stations de référence", uiOutput(ns("reference")))),
                                   leafletOutput(ns("carte"))
                                   
                  ))
        
  )
}

#' complexe Server Functions
#'
#' @noRd 
mod_complexe_server <- function(id){
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
    #
    output$test <- renderDataTable({
      input$shape
    })
    #Cacher les tab panel
    # beaucoup de code, ameliorer ca ?
    
    #tab 2
    observeEvent(req(as.numeric(zones()) < 2), {
      hideTab(inputId = "tab", target = "Sous zone 2")
    })
    observeEvent(req(as.numeric(zones()) > 1), {
      showTab(inputId = "tab",
              target = "Sous zone 2",
              select = T)
    })
    #tab 3
    observeEvent(req(as.numeric(zones()) < 3), {
      hideTab(inputId = "tab", target = "Sous zone 3")
    })
    observeEvent(req(as.numeric(zones()) > 2), {
      showTab(inputId = "tab",
              target = "Sous zone 3",
              select = T)
    })
    #tab 4
    observeEvent(req(as.numeric(zones()) < 4), {
      hideTab(inputId = "tab", target = "Sous zone 4")
    })
    observeEvent(req(as.numeric(zones()) > 3), {
      showTab(inputId = "tab",
              target = "Sous zone 4",
              select = T)
    })
    #tab 5
    observeEvent(req(as.numeric(zones()) < 5), {
      hideTab(inputId = "tab", target = "Sous zone 5")
    })
    observeEvent(req(as.numeric(zones()) > 4), {
      showTab(inputId = "tab",
              target = "Sous zone 5",
              select = T)
    })
    #tab 6
    observeEvent(req(as.numeric(zones()) < 6), {
      hideTab(inputId = "tab", target = "Sous zone 6")
    })
    observeEvent(req(as.numeric(zones()) > 5), {
      showTab(inputId = "tab",
              target = "Sous zone 6",
              select = T)
    })
    #tab 7
    observeEvent(req(as.numeric(zones()) < 7), {
      hideTab(inputId = "tab", target = "Sous zone 7")
    })
    observeEvent(req(as.numeric(zones()) > 6), {
      showTab(inputId = "tab",
              target = "Sous zone 7",
              select = T)
    })
    #tab 8
    observeEvent(req(as.numeric(zones()) < 8), {
      hideTab(inputId = "tab", target = "Sous zone 8")
    })
    observeEvent(req(as.numeric(zones()) > 7), {
      showTab(inputId = "tab",
              target = "Sous zone 8",
              select = T)
    })
    
    # Modules pour la variables traitement selon le nombre de zones (rep le module 8 fois pour max 8 zones)
    zones <- reactive({
      input$zones
    })
    ls1 <- traitementServer("zone1", tutti_operation(), "Sous zone 1")
    ls2 <- traitementServer("zone2", tutti_operation(), "Sous zone 2")
    ls3 <- traitementServer("zone3", tutti_operation(), "Sous zone 3")
    ls4 <- traitementServer("zone4", tutti_operation(), "Sous zone 4")
    ls5 <- traitementServer("zone5", tutti_operation(), "Sous zone 5")
    ls6 <- traitementServer("zone6", tutti_operation(), "Sous zone 6")
    ls7 <- traitementServer("zone7", tutti_operation(), "Sous zone 7")
    ls8 <- traitementServer("zone8", tutti_operation(), "Sous zone 8")
    
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
    
    # Représentation des stations
    tutti_operation_bis <-
      reactive({
        pre_leaflet(tutti_operation(), ls_station(), ls_dates(), zones())
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
        zones()
      )
    })
    
 list(
    input$go,
    data_forme())

    
  })
}
    
## To be copied in the UI
# mod_complexe_ui("complexe_ui_1")
    
## To be copied in the server
# mod_complexe_server("complexe_ui_1")
