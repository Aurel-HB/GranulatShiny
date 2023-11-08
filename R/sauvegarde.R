#mainPanel(
#verbatimTextOutput("testprint"),
#mod_simple_02_ui("simple_02_ui_1"),
#tabBox(id = "tab_reference",
#        tabPanel(
#         "Stations de référence", uiOutput("reference")
#         )),
#leafletOutput("carte")
#)






##### check point ######
#output$summary <- renderPrint({
#  if(is.null(tutti_catch())){return("No data")}
#  summary(tutti_catch())
#})
########################

# try the next part of the app #####

## table des stations de référence
#output$reference <- renderUI({
#  checkboxGroupInput(
#    "reference",
#    label = "Si besoin, séléctionner les stations à retirer :",
#    choices = unique(tutti_operation_bis()[which(is.na(tutti_operation_bis()$impact_date_1)), ]$Code_Station),
#    inline = T
#  )
#})
#Les stations à retirer
#station_ban <- reactive({
#  input$reference
#})

# Zones (fixer le nombre de zone à 1 pour le mode simple)
#zones <- reactive({
#  1
#})
#
#observe({
#  callModule(mod_simple_02_server, id = "simple_02_ui_1", session = session, r=r, operation = tutti_operation(), test = upload())
#})
#module <- reactive({
#  r$module
#})

## sauvegarde des inputs
#save <- reactive({
#  save <- data.frame(
#    stations = as.character(NA),
#    dates_deb = as.Date(NA),
#    dates_fin = as.Date(NA),
#    ban = as.character(NA),
#    stringsAsFactors = FALSE
#  )
#  save$stations <-
#    paste(as.character(r$module[[1]][[1]]), collapse = "/")
#  save$dates_deb <- r$module[[2]][[1]][1]
#  save$dates_fin <- r$module[[2]][[1]][2]
#  save$ban <- paste(as.character(station_ban()), collapse = "/")
#  save
#})


## Telecharger la sauvegarde
#output$downloadSave <- downloadHandler(
#  filename = function() {
#    paste("save-input", ".csv", sep = "")
#  },
#  content = function(file) {
#    write.csv(save(),  file)
#  }
#)


# Représentation des stations : prepare un dataframe pour la carte leaflet
#tutti_operation_bis <-
#  reactive({
#    pre_leaflet(tutti_operation(), r$module[[1]], r$module[[2]], zones())
#  })
#
## filtrer les stations à retirer
#tutti_operation_filtre <- reactive({
#  if (is.null(station_ban())) {
#    tutti_operation_bis()
#  } else {
#    tutti_operation_bis()[-which(tutti_operation_bis()$Code_Station %in% station_ban()), ]
#  }
#})

#tutti_catch_filtre <- reactive({
#  if (is.null(station_ban())) {
#    tutti_catch()
#  } else {
#    tutti_catch()[-which(tutti_catch()$Trait %in% station_ban()), ]
#  }
#})







########################
##' traitement UI Function
##'
##' description Module qui permet d'afficher les inputs necessaires à la création de la variabe traitement :
##' les stations d'impact et les dates associées. le module retourne une liste de ces infos.
##'
##' param id,input,output,session Internal parameters for {shiny}.
##'
##' noRd
##'
##' importFrom shiny NS tagList
#mod_traitement_ui <- function(id) {
#  ns <- NS(id)
#  tagList(
#    uiOutput(ns("datesOut")),
#    uiOutput(ns("stationOut")),
#    selectInput(
#      ns("complex"),
#      "Prendre en compte la stratégie d'extraction ?",
#      c("non" = "1", "oui" = "2")
#    ))
#
#}
#
##' traitement Server Functions
##'
##' noRd
#mod_traitement_server <- function(input, output, session, tutti_operation, label, test, r) {
#                 ns <- session$ns
#                 test
#                 output$numero <- renderText({
#                   label
#                 })
#                 output$datesOut <-
#                   renderUI({
#                     dateRangeInput(
#                       ns("dates"),
#                       "Dates de la période d'activité :",
#                       start = min(as.Date(
#                         tutti_operation$DateDeb,  "%d/%m/%Y"
#                       )),
#                       end = max(as.Date(
#                         tutti_operation$DateDeb,  "%d/%m/%Y"
#                       )),
#                       format = "yyyy-mm-dd",
#                       startview = "month",
#                       weekstart = 0,
#                       language = "fr",
#                       separator = " jusqu'au "
#                     )
#                   })
#                 output$stationOut <- renderUI({
#                   selectInput(
#                     ns("station"),
#                     "Stations à l'intérieur de la zone d'étude :",
#                     multiple = T,
#                     choices = levels(as.factor(tutti_operation$Code_Station))
#                   )
#                 })
#
#                 observeEvent(test, {
#                   updateSelectInput(inputId =  ns("station"), selected = test[[1]][[1]])
#                   updateDateRangeInput(inputId = ns("dates"), start = test[[2]][[1]], end = test[[3]][[1]] )
#                 })
#                 reactive({
#                   list(
#                     input$station,
#                     input$dates
#                   )
#                 })
#
#                 observe({
#                   r$ls1 <- list()
#                 })
#
#
#}
#
### To be copied in the UI
## mod_traitement_ui("traitement_ui_1")
#
### To be copied in the server
## mod_traitement_server("traitement_ui_1")



#' Tables UI Function
#'
#' description A shiny Module.
#'
#' param id,input,output,session Internal parameters for {shiny}.
#'
##' noRd
##'
##' importFrom shiny NS tagList
#mod_Tables_ui <- function(id){
#  ns <- NS(id)
#  tagList(
#    sidebarPanel(
#      selectInput(
#        ns("list"),
#        "Quelle table afficher ?",
#        c(
#          "Table complete" = "1",
#          "Abondance" = "2",
#          "Biomasse" = "3",
#          "Richesse" = "4",
#          "Simpson" = "5"
#        ),
#        selected = "Table complete"
#      ),
#      downloadButton(ns("downloadData"), label = "Telecharger la table"),
#      hr(),
#      downloadButton(ns("downloadSave"), label = "Telecharger les informations rentrées"),
#      hr(),
#      uiOutput(ns("variable")),
#      actionButton(ns("stat"), "Commencer l'analyse")
#      #verbatimTextOutput(ns("name"))
#    ),
#    mainPanel(
#      conditionalPanel(
#        "input.go != false",
#        box(
#          dataTableOutput(ns("contents")),
#          width = NULL,
#          style = "overflow-x: scroll;",
#          collapsible = T,
#          solidHeader = TRUE
#        )
#      ))
#  )
#}
#
##' Tables Server Functions
##'
##' @noRd
#mod_Tables_server <- function(input, output, session, r){
#  ns <- session$ns
#
#  save <- reactive({
#    r$save
#  })
#
#  data_forme <- reactive({
#    r$data_forme
#  })
#
#  # Telecharger la sauvegarde
#  output$downloadSave <- downloadHandler(
#    filename = function() {
#      paste("save-input", ".csv", sep = "")
#    },
#    content = function(file) {
#      write.csv(save(),  file)
#    }
#  )
#
#  variables <- reactive({
#    n <- names(as.data.frame(data_forme()[[1]]))
#    var_expl <- c("year","station","date","saison","campagne","tow","hauled_surf",
#                  "traitement","interaction","time") # filter the explanatory variable
#    n <- n[!(n %in% var_expl)] # keep only the explicated value
#    n
#  })
#
#  output$variable <- renderUI({
#    selectInput(
#      ns("var"),
#      "Choisir un variable pour l'analyse",
#      choices = levels(as.factor(variables())),
#      selected = "Abun"
#    )
#  })
#
#  data_analyse <- reactive({
#    if(is.null(data_forme())){return()}
#    v <- data_forme()[[1]][input$var]
#    y <- data_forme()[[1]]["year"]
#    l <- data_forme()[[1]]["station"]
#    s <- data_forme()[[1]]["saison"]
#    t <- data_forme()[[1]]["traitement"]
#    i <- data_forme()[[1]]["interaction"]
#    data <- data.frame(v, y, l, s, t, i)
#    names(data) <- c(input$var, "year", "station", "saison", "traitement", "interaction")
#    data
#  })
#
#  #### check point ####
#  #output$name <- renderPrint({
#  #  if(is.null(data_analyse())){return("No data")}
#  #  summary(data_analyse())
#  #})
#  #####################
#
#  #outPut de la table
#  output$contents <- renderDataTable({
#    #if(is.null(data_forme())){return()}
#    data_forme()[[as.integer(input$list)]]
#
#  })
#
#  #Telecharger les données
#  nom_data <-
#    c("Table complete",
#      "Abondance",
#      "Biomasse",
#      "Richesse",
#      "Simpson")
#
#  output$downloadData <- downloadHandler(
#    filename = function() {
#      paste("data-", nom_data[as.integer(input$list)], ".csv", sep = "")
#    },
#    content = function(file) {
#      write.csv(data_forme()[[as.integer(input$list)]],  file)
#    }
#  )
#
#  observe({
#    r$data_analyse <- data_analyse()
#  })
#
#  button2 <- eventReactive(
#    input$stat, {TRUE
#    })
#
#  observe({
#    r$button2 <- button2()
#  })
#
#}

## To be copied in the UI
# mod_Tables_ui("Tables_1")

## To be copied in the server
# mod_Tables_server("Tables_1")
