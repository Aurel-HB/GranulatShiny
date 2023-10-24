#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import shiny
#' @import tidyr
#' @import vegan
#' @import leaflet
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @import lme4
#' @import car
#' @import stats
#' @import MASS
#' @import DHARMa
#' @import sjPlot
#' @import stringr
#' @import multcomp
#' @import multcompView
#' @import lsmeans
#' @import simr
#' @import RColorBrewer
#' @import plyr
#' @import sf
#' @noRd

app_server <- function(input, output, session) {
  # Mise en forme -----------------------------------------------------------
  
  # pour faire marcher conditionalpanel dans le UI (les inputs s'affichent une fois que le fichier operation est chargé)
  
  output$operation_charge <- reactive({
    return(!is.null(input$tutti_operation))
  })
  
  outputOptions(output, "operation_charge", suspendWhenHidden = FALSE)
  
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
    polygon <-
      shape[["geometry"]][[1]] ## [[1]] pour SIEGMA [[2]] pour RESISTE, possible de rajouter un input pour le choix de la ligne pour les shapefiles avec plusieurs lignes
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
      polygon <- shpFile[["geometry"]][[1]] # [[1]] pour SIEGMA [[2]] pour RESISTE
      return(polygon)
    } else {
      return()
    }
  })
  
  # table des stations de référence
  output$reference <- renderUI({
    checkboxGroupInput(
      "reference",
      label = "Si besoin, séléctionner les stations à retirer :",
      choices = unique(tutti_operation_bis()[which(is.na(tutti_operation_bis()$impact_date_1)), ]$Code_Station),
      inline = T
    )
  })
  #Les stations à retirer
  station_ban <- reactive({
    input$reference
  })
  
  # Zones (fixer le nombre de zone à 1 pour le mode simple)
  zones <- reactive({
    if (input$complex == "1") {
      1
    } else {
      input$zones
    }
  })
  
  # Charger la sauvegarde
  upload <- reactive({
    if (is.null(input$uploadSave)) {
      NULL
    } else {
      pre_file_save <- input$uploadSave[, 4]
      file_save <- read.csv (pre_file_save, header = T, sep = ",")
      station <- strsplit(as.character(file_save$stations), split = "/")
      dates_deb <- file_save$dates_deb
      dates_fin <- file_save$dates_fin
      ban <- strsplit(as.character(file_save$ban), split = "/")
      list_save <- list(station, dates_deb, dates_fin, ban)
      return(list_save)
    }
  })
  
  #update suite au chargement de la sauvergarde des stations à ban (le reste des udates sont dans le module traitement)
  observeEvent(r$module[[1]], {
    # recharge la sauvegarde à chaque modif des stations d'impact car sinon les informations s'effacent
    updateCheckboxGroupInput(inputId = "reference", selected = upload()[[4]][[1]])
  })
  
  # reactiveValues créée pour sauvegarder les infos des modules
  r <- reactiveValues()
  
  # Choix de la complexité
  
  observe(if (input$complex == "1") {
    #Module mise en forme des données simple
    mod_simple_02_server("simple_02_ui_1",  tutti_operation(), r, upload())
  } else {
    # Module mise en forme des données complexe (en dev)
    mod_complexe_02_server("complexe_02_ui_1",  tutti_operation(), r, upload())
  })
  
  
  
  # sauvegarde des inputs
  save <- reactive({
    save <- data.frame(
      stations = as.character(NA),
      dates_deb = as.Date(NA),
      dates_fin = as.Date(NA),
      ban = as.character(NA),
      stringsAsFactors = FALSE
    )
    save$stations <-
      paste(as.character(r$module[[1]][[1]]), collapse = "/")
    save$dates_deb <- r$module[[2]][[1]][1]
    save$dates_fin <- r$module[[2]][[1]][2]
    save$ban <- paste(as.character(station_ban()), collapse = "/")
    save
  })
  
  
  # Telecharger la sauvegarde
  output$downloadSave <- downloadHandler(
    filename = function() {
      paste("save-input", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(save(),  file)
    }
  )
  
  
  #Cacher les tab panel du module complex (répétition du module traitement)
  # beaucoup de code, ameliorer ca ?
  
  #tab 2
  observeEvent(req(as.numeric(zones()) < 2), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 2")
  })
  observeEvent(req(as.numeric(zones()) > 1), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 2",
      select = T
    )
  })
  #tab 3
  observeEvent(req(as.numeric(zones()) < 3), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 3")
  })
  observeEvent(req(as.numeric(zones()) > 2), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 3",
      select = T
    )
  })
  #tab 4
  observeEvent(req(as.numeric(zones()) < 4), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 4")
  })
  observeEvent(req(as.numeric(zones()) > 3), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 4",
      select = T
    )
  })
  #tab 5
  observeEvent(req(as.numeric(zones()) < 5), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 5")
  })
  observeEvent(req(as.numeric(zones()) > 4), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 5",
      select = T
    )
  })
  #tab 6
  observeEvent(req(as.numeric(zones()) < 6), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 6")
  })
  observeEvent(req(as.numeric(zones()) > 5), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 6",
      select = T
    )
  })
  #tab 7
  observeEvent(req(as.numeric(zones()) < 7), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 7")
  })
  observeEvent(req(as.numeric(zones()) > 6), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 7",
      select = T
    )
  })
  #tab 8
  observeEvent(req(as.numeric(zones()) < 8), {
    hideTab(inputId = NS("complexe_02_ui_1" , "tab"),
            target = "Sous zone 8")
  })
  observeEvent(req(as.numeric(zones()) > 7), {
    showTab(
      inputId = NS("complexe_02_ui_1" , "tab"),
      target = "Sous zone 8",
      select = T
    )
  })
  
  #   # Représentation des stations : prepare un dataframe pour la carte leaflet
  tutti_operation_bis <-
    reactive({
      pre_leaflet(tutti_operation(), r$module[[1]], r$module[[2]], zones())
    })
  
  # filtrer les stations à retirer
  tutti_operation_filtre <- reactive({
    if (is.null(station_ban())) {
      tutti_operation_bis()
    } else {
      tutti_operation_bis()[-which(tutti_operation_bis()$Code_Station %in% station_ban()), ]
    }
  })
  
  tutti_catch_filtre <- reactive({
    if (is.null(station_ban())) {
      tutti_catch()
    } else {
      tutti_catch()[-which(tutti_catch()$Trait %in% station_ban()), ]
    }
  })
  #leaflet
  output$carte <- renderLeaflet({
    carte <- leaflet_maker(tutti_operation_filtre(), shape())
  })
  
  
  # Mettre en forme et calcul des indicateurs
  data_forme <- eventReactive(input$go, {
    global_function(tutti_catch_filtre(), # fonction qui englobe toutes les autres
                    tutti_operation_filtre(),
                    r$module[[1]],
                    r$module[[2]],
                    zones())
  })
  
  # Onglet Tables
  
  # Ouvre l'onglet Tables automatiquement
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "table")
  })
  
  #outPut de la table
  output$contents <- renderDataTable({
    data_forme()[[as.integer(input$list)]]
    
  })
  
  
  #Telecharger les données
  nom_data <-
    c("Table complete",
      "Abondance",
      "Biomasse",
      "Richesse",
      "Simpson")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", nom_data[as.integer(input$list)], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_forme()[[as.integer(input$list)]],  file)
    }
  )


  # GLMMs -------------------------------------------------------------------
  # Onglet Creation
  
  # Render UI parametrage glmms
  
  output$variable_y <-
    renderUI({
      selectInput("y", "Choississez la variable Y", choices = colnames(data_forme()[[1]])[-c(1:9)])
    })
  
  output$covariable <-
    renderUI({
      selectInput(
        "covariable",
        "Ajouter une ou plusieurs covariables ?",
        choices = colnames(data_forme()[[1]]),
        selected = F,
        multiple = T
      )
    })
  
  # Mise en forme des expression reactives
  y_variable <- reactive({
    input$y
  })
  data_complet <- reactive({
    data_forme()[[1]]
  })
  
  colonne_y <-
    reactive({
      unlist(data_complet()[, y_variable()])
    }) # sinon ggplot ne marche pas pour l'histogramme
  
  interaction <- reactive({
    input$interaction
  })
  methode <- reactive({
    input$methode
  })
  loi <- reactive({
    input$loi
  })
  #Ggplot pour visualiser la variable Y
  output$plot_y <-
    renderPlot({
      ggplot(data = data_complet(), aes(x = colonne_y())) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
        geom_density(alpha = .2, fill = "blue") +
        xlab(y_variable()) +
        theme_minimal()
    })
  #ecriture du modèle initial
  
  ecriture <-
    reactive({
      ecriture_modele(y_variable(),
                      input$interaction,
                      input$methode,
                      input$covariable,
                      input$loi)
    })
  output$ecriture_modele <- renderText(ecriture()[[1]])
  output$ecriture_loi <- renderText(ecriture()[[2]])
  # formule et language modèle
  formule <- reactive({
    ecriture()[[3]]
  })
  formule_bis <- reactive({
    ecriture()[[4]]
  }) # formule sans interaction
  language <-
    reactive({
      ecriture()[[5]]
    }) #permet de bien nommer les variabes pour les GLM, necessaire pour que les test de puissances marchent
  language_bis <- reactive({
    ecriture()[[6]]
  })
  formule_interaction <- reactive({
    ecriture()[[7]]
  })
  language_interaction <- reactive({
    ecriture()[[8]]
  })
  #Modélo différentes méthodes
  
  modele <- eventReactive(input$go2, {
    if (input$methode == "1") {
      glmm_maker(data_complet(),
                 formule(),
                 formule_bis(),
                 input$interaction,
                 input$loi)
    } else if (input$methode == "2")  {
      glm_maker(
        data_complet(),
        formule(),
        formule_bis(),
        language(),
        language_bis(),
        input$interaction,
        input$loi
      )
    } else if (input$methode == "3") {
      permanova_maker(data_complet(),
                      formule(),
                      formule_bis(),
                      input$interaction)
    }
  })
  
  #Afficher le choix du modèle entre initial et finalseulement si l'interaction n'est pas significative (et donc retirée)
  
  output$choix_modele <- renderUI({
    if (getCall(modele()[[2]]) != getCall(modele()[[1]])) {
      selectInput(
        "choix_modele",
        "Afficher les résultats du modèle :",
        c("initial" = "1", "final" = "2"),
        selected = "2"
      )
    }
  })
  
  choix_modele <- reactive({
    if (!is.null(input$choix_modele)) {
      as.integer(input$choix_modele)
    } else {
      2
    }
  })
  
  # Les sorties du modèle
  output$modele <- renderPrint({
    if (methode() %in% c(1, 2)) {
      if (input$choix_sortie == "1") {
        Anova(modele()[[choix_modele()]], type = "III")
      } else if (input$choix_sortie == "2") {
        summary(modele()[[choix_modele()]])
      }
      
      
    } else {
      if (input$choix_sortie == "1") {
        modele()[[choix_modele()]]$aov.tab
      } else if (input$choix_sortie == "2") {
        modele()[[choix_modele()]]$coefficients
      }
      
      
    }
  })
  
  # Vérification (sortie du modèle, même input)
  output$verification <- renderPlot({
    if (input$choix_sortie == "3") {
      plot(simulateResiduals(modele()[[choix_modele()]]))
    }
  })
  
  # Visualisation des effets
  # Render UI pour la visualisation
  output$effet_choix <-
    renderUI({
      if (is.na(Anova(modele()[[2]], type = "II")["traitement:saison", 1]) == F) {
        selectInput(
          "effet_choix",
          "Choississez l'effet à représenter",
          choices = c("traitement*saison" = "traitement_saison", input$covariable)
        )
      } else {
        selectInput(
          "effet_choix",
          "Choississez l'effet à représenter",
          choices = c("traitement", "saison", input$covariable)
        )
        
      }
    })
  
  # plot_model : fonction pour representer graphiquement les effets marginaux 
  plot_effet <- reactive({
    plot_model(modele()[[2]], type = "pred") #(verifier si l'argument type est le bon, peut influer sur le niveau moyen de la variable Y)
  })
  plot_effet_int <-
    reactive({
      plot_model(modele()[[2]], type = "int") #(verifier si l'argument type est le bon, peut influer sur le niveau moyen de la variable Y)
    })
  effet_choix <- reactive({
    if (input$effet_choix == "traitement_saison") {
      effet_choix <- c("traitement", "saison")
    } else{
      effet_choix <- input$effet_choix
    }
  })
  
  #test post-hoc entre les modalités
  lettre <- reactive({
    if (input$post_hoc == T) {
      data_group <-
        as.data.frame(cld(
          lsmeans(modele()[[2]], effet_choix()),
          alpha = 0.05,
          Letters = letters,
          adjust = "fdr"
        ))
      if (length(effet_choix()) == 2) {
        lettre_group <-
          data_group[order(data_group$traitement, data_group$saison),]$.group
      } else {
        lettre_group <- data_group$.group
      }
    } else {
      lettre_group <- ""
    }
  })
  
  #Plot des effets avec lettre post hoc
  plot_effet_final <- reactive({
    if (length(effet_choix()) == 2) {
      plot_effet_int() +
        geom_text(
          aes(label = lettre()),
          hjust = input$largeur_plot,
          vjust = input$hauteur_plot,
          size = 5,
          check_overlap = T,
          col = "black"
        ) +
        ggtitle("") +
        theme_classic()
    } else {
      plot_effet()[effet_choix()][[1]] +
        ggtitle("") +
        theme_classic() +
        geom_text(
          aes(label = lettre()),
          size = 5,
          check_overlap = T,
          hjust = input$largeur_plot,
          vjust = input$hauteur_plot,
          col = "black"
        )
      
    }
  })
  
  output$effet_plot <- renderPlot({
    plot_effet_final()
  })
  # Exporter le graphique
  output$downloadPlot_effet <- downloadHandler(
    filename = function() {
      paste("plot-", input$effet_choix, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_effet_final())
    }
  )
  
  
  # Test de puissance 
  # Render UI
  output$puissance_choix <- renderUI({
    selectInput(
      "puissance_choix",
      "Choississez le terme à tester",
      choices = c(
        "traitement:saison" = "interaction",
        "traitement",
        "saison",
        input$covariable
      )
    )
  })
  
  #fonction pour la puissance
  
  puissance <- eventReactive(input$go3, {
    if (methode() == "1") {
      modele_puissance <-
        glmm_maker_puissance(data_complet(),
                             formule_interaction(),
                             formule_bis(),
                             loi())
    } else if (methode() == "2")  {
      modele_puissance <-
        glm_maker_puissance(
          data_complet(),
          formule_interaction(),
          formule_bis(),
          language_interaction(),
          language_bis(),
          loi()
        )
    }
    if (input$puissance_choix == "interaction") {
      puissance_maker(
        modele_puissance[[1]],
        input$puissance_choix,
        input$nbr_campagne,
        max(data_complet()$campagne),
        methode(),
        data_complet()
      )
    } else {
      puissance_maker(
        modele_puissance[[2]],
        input$puissance_choix,
        input$nbr_campagne,
        max(data_complet()$campagne),
        methode(),
        data_complet()
      )
    }
  })
  
  
  output$plot_puissance <- renderPlot({
    plot(puissance())
  })
  
}
