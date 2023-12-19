#' Import_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Import_data_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectInput(
        ns("complex"),
        "Prendre en compte la stratégie d'extraction ?",
        c("non" = "1", "oui" = "2")
      ),
      fileInput(ns("tutti_catch"), "Sélectionnez le fichier Tutti Catch (.csv)", accept = c(".csv")),
      fileInput(ns("tutti_operation"),
                "Sélectionnez le fichier Tutti operation (.csv)", accept = c(".csv")),
      uiOutput(ns("shpFile")),
      uiOutput(ns("uploadSave")),
      hr()

  )
}

#' Import_data Server Functions
#'
#' @noRd
mod_Import_data_server <- function(input, output, session, r){
    ns <- session$ns
    # Mise en forme -----------------------------------------------------------
    # pour faire marcher conditionalpanel dans le UI (les inputs s'affichent une fois que le fichier operation est chargé)


    format_catch <- c("Campagne","Annee","Trait","Nom_Scientifique","Code_Campagne",
                "Nombre","Poids","Pmoy","longueurmoy","DateDeb","LatDeb",
                "LongDeb","DateFin","LatFin","LongFin")

    # Charger les données
    tutti_catch <- reactive({
      if(is.null(input$tutti_catch)){return()}
      file1 <- input$tutti_catch[,4]
      if(grepl(".csv",file1) == FALSE){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = "Le fichier n'a pas le bon format !
          Référez vous au format tutti_catch de la notice.",
          type = "fail"
        )
        return()
      }
      data11 <- read.csv (file1, header = T, sep = ";")
      if (length(data11)==1){
        data11 <- read.csv (file1, header = T, sep = ",")
      }
      verif <- verification(names(data11), format_catch)
      if(verif == FALSE){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = "Le fichier n'a pas le bon format !
          Référez vous au format tutti_catch de la notice.",
          type = "fail"
        )
        return()
      }
      data11
    })

    observe({
      r$tutti_catch <- tutti_catch()
    })

    format_operation <- c("Annee","Serie","Serie_Partielle","Code_Station",
                          "Id_Operation","DateDeb","LatDeb","LongDeb","DateFin",
                          "LatFin","LongFin","Distance")

    tutti_operation <- reactive({
      if(is.null(input$tutti_operation)){return()}
      file2 <- input$tutti_operation[,4]
      if(grepl(".csv",file2) == FALSE){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = "Le fichier n'a pas le bon format !
          Référez vous au format tutti_operation de la notice.",
          type = "fail"
        )
        return()
      }
      data22 <- read.csv (file2, header = T, sep = ";")
      if (length(data22)==1){
        data22 <- read.csv (file2, header = T, sep = ",")
      }
      verif <- verification(names(data22), format_operation)
      if(verif == FALSE){
        sendSweetAlert(
          session = session,
          title = "Alert !",
          text = "Le fichier n'a pas le bon format !
          Référez vous au format tutti_operation de la notice.",
          type = "fail"
        )
        return()
      }
      data22
    })

    observe({
      r$tutti_operation <- tutti_operation()
    })


    # Shape file

    output$shpFile <- renderUI({
      if(is.null(tutti_catch())){return()}
      if(is.null(tutti_operation())){return()}
      fileInput(
        ns("shpFile"),
        "Sélectionnez les Shapefiles (.shp, .shx, .dbf)", accept = c(".shp", ".shx", ".dbf"),
        multiple = T
      )
    })

    # deletion of useless command on the shape ###

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


        # Check the number of features/entities in the shapefile
        num_entities <- nrow(shpFile)
        # Print the result
        if (num_entities != 1) {
          sendSweetAlert(
            session = session,
            title = "Alert !",
            text = "Le fichier n'a pas le bon format !
          Référez vous au format shapefile de la notice.",
            type = "fail"
          )
        }

        polygon <- shpFile[["geometry"]][[1]] # [[1]] pour SIEGMA [[2]] pour RESISTE,
        # possible de rajouter un input pour le choix de la ligne pour les shapefiles avec plusieurs lignes
        return(polygon)
      } else {
        return()
      }
    })


    # Charger la sauvegarde

    output$uploadSave <- renderUI({
      if(is.null(tutti_catch())){return()}
      if(is.null(tutti_operation())){return()}
      fileInput(ns("uploadSave"), "Charger les informations si vous avez une
                    sauvegarde des paramètres de la zone étudiée (.csv)",
                accept = c(".csv"))
    })

    format_sauvegarde <- c("","stations","dates_deb","dates_fin","ban")

    upload <- reactive({
      if (is.null(input$uploadSave)) {
        NULL
      } else {
        pre_file_save <- input$uploadSave[, 4]
        if(grepl(".csv",pre_file_save) == FALSE){
          sendSweetAlert(
            session = session,
            title = "Alert !",
            text = "Le fichier n'a pas le bon format !
          Référez vous au format sauvegarde de la notice.",
            type = "fail"
          )
          return()
        }
        file_save <- read.csv (pre_file_save, header = T, sep = ",")
        verif <- "dates_deb" %in% names(file_save)
        if(verif == FALSE){
          sendSweetAlert(
            session = session,
            title = "Alert !",
            text = "Le fichier n'a pas le bon format !
          Référez vous au format sauvegarde de la notice.",
            type = "fail"
          )
          return()
        }
        station <- strsplit(as.character(file_save$stations), split = "/")
        dates_deb <- file_save$dates_deb
        dates_fin <- file_save$dates_fin
        ban <- strsplit(as.character(file_save$ban), split = "/")
        list_save <- list(station, dates_deb, dates_fin, ban)
        return(list_save)
      }
    })


    observe({
      r$shape <- shape()
    })

    observe({
      r$upload <- upload()
    })

}

## To be copied in the UI
# mod_Import_data_ui("Import_data_1")

## To be copied in the server
# mod_Import_data_server("Import_data_1")
