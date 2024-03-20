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
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
      box( title = i18n$t("Résumé"),
           status = "success", #  Valid statuses are: primary, success, info, warning, danger.
           solidHeader = TRUE,
           uiOutput(ns("variable")),
           width = 12,
           actionButton(ns("info2"), "",icon = icon("circle-info")),
           tableOutput(ns("var_summary")),
        #tableOutput(ns("var_summary")),
        #plotOutput(ns("hist")),
        hr(),
        h4(i18n$t("Boite à moustache")),
        uiOutput(ns("choix_box")),
        actionButton(ns("info4"), "",icon = icon("circle-info")),
        plotOutput(ns("boxplot"), width = "60%"),
        downloadButton(ns("downloadPlot"),
                       label = i18n$t("Telecharger le graphique (.png)")),
        checkboxInput(ns("outlier"), i18n$t("Voulez-vous retirer les valeurs extrêmes ?")),
        checkboxInput(ns("log"), i18n$t("Voulez-vous passer au log ?")),
        textOutput(ns("info")),
        hr(),
        actionButton("goloi", i18n$t("Passer au choix de la loi de distribution"),
                      icon = icon("ship"))
      )#,
      #### old version #####
      #box( title = "Interactionplot",
      #     solidHeader = TRUE,
      #     status = "info",
      #     collapsible = TRUE,
      #     collapsed = TRUE,
      #     style = "overflow-x: scroll;",
      #     uiOutput(ns("choix_interaction")),
      #     actionButton(ns("info3"), "",icon = icon("circle-info")),
      #     plotOutput(ns("interact"))
      #),
      #box( title = "Boxplot",
      #     solidHeader = TRUE,
      #     status = "info",
      #  collapsible = TRUE,
      #  collapsed = TRUE,
      #  uiOutput(ns("choix_box")),
      #  actionButton(ns("info4"), "",icon = icon("circle-info")),
      #  plotOutput(ns("boxplot"))
      #)
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
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    data_forme <- reactive({
      r$data_forme
    })

    variables <- reactive({
      n <- names(as.data.frame(data_forme()[[1]]))
      var_expl <- c("year","station","date","saison","campagne","tow","hauled_surf",
                    "traitement","interaction","time") # filter the explanatory variable
      n <- n[!(n %in% var_expl)] # keep only the explicated value
      n
    })

    output$variable <- renderUI({
      if(is.null(data_forme())){return()}
      selectInput(
        ns("var"),
        i18n$t("Choisir un variable pour les statistiques descriptives"),
        choices = levels(as.factor(variables())),
        selected = "Abun"
      )
    })

    data_analyse <- reactive({
      if(is.null(data_forme())){return()}
      if(is.null(input$var)){return()}
      # to avoid bug when you switch with another concession data :
      if((input$var %in% names(data_forme()[[1]]))== FALSE){return()}
      # to levels the season in the chronological order
      permutation <- order(data_forme()[[1]]$date)
      permuted <-data_forme()[[1]][permutation,]

      v <- data_forme()[[1]][input$var]
      y <- data_forme()[[1]]["year"]
      l <- data_forme()[[1]]["station"]
      s <- factor(data_forme()[[1]]$saison,
                  unique(permuted$saison))
      t <- data_forme()[[1]]["traitement"]
      #i <- data_forme()[[1]]["interaction"]
      c <- data_forme()[[1]]["campagne"]
      data <- data.frame(v, y, l, s, t, c) #i,c)
      names(data) <- c(input$var, "year", "station", "saison", "traitement", "campagne") #"interaction", "campagne")
      data
    })

    observe({
      r$var_name <- input$var
    })

    observe({
      r$data_analyse <- data_analyse()
    })


    output$info <- renderText({
      return(
      i18n$t("Attention si votre variable contient des valeurs manquantes ou des zéros vous ne pourrez pas utiliser de transformation log (loi lognormale) dans le modèle.")
      )
    })

    ##### information #####
    observeEvent(input$info2,{
      message <- as.character(list_translate[r$lang][7,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info3,{
      message <- as.character(list_translate[r$lang][8,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info4,{
      message <- as.character(list_translate[r$lang][9,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })
    ######

    # variable is the vector of the chosen variable with or without transformation
    variable <- reactive({
      if(is.null(data_analyse())){return()}
      if (input$log && input$outlier){
        return(log(delete_outliers(as.numeric(data_analyse()[,1]))+1))
      }
      else if (input$outlier){
        return(delete_outliers(as.numeric(data_analyse()[,1])))
      }
      else if (input$log){
        return(log(as.numeric(data_analyse()[,1]+1)))
      } else {
        return(as.numeric(data_analyse()[,1]))
      }
    })

    # data_variable follow the change of variable
    # if the outliers are deleted, the lines corresponding are removed for the plots
    data_variable <- reactive({
      if(is.null(data_analyse())){return()}
      if (input$outlier){
      return(as.data.frame(data_analyse()
                           %>% dplyr::filter(data_analyse()[,1] %in%
                                               delete_outliers(as.numeric(data_analyse()[,1])))))
      }
      data_analyse()
    })



    output$var_summary <- renderTable({
      if(is.null(data_analyse())){return()}
      numeric_summary(as.numeric(data_analyse()[,1]), names(data_analyse())[1])
    }) # the summary is made on the variable without transformation

    #### boxplot part ####
    output$choix_box <- renderUI({
      selectInput(
        ns("choix_box"),
        i18n$t("Sélectionner la covariable :"),
        c("traitement" = "1",
          "year" = "2",
          "campagne" = "3",
          "station" = "4",
          "saison" = "5"),
        selected = "1"
      )
    })

    create_boxplot <- reactive({
      if(is.null(data_analyse())){return()}
      if(is.null(input$choix_box)){return()}
      if(is.null(variable())){return()}
      if (input$log){
        var_name <- paste("log(", r$var_name,")", sep="")
      } else{ var_name <- r$var_name}
      if(input$choix_box == 1){
        plot <- ggplot(data_variable(), aes(x = traitement, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by impact", sep=""),
          y = "")#+ add calculate of the mean
          #stat_summary(fun.y = mean, geom = "point",
          #             shape = 18, size = 2.5, color = "#FC4E07")
      }
      if(input$choix_box == 2){
        plot <- ggplot(data_variable(), aes(x = year, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by year", sep=""),
               y = "")#+ add calculate of the mean
        #stat_summary(fun.y = mean, geom = "point",
        #             shape = 18, size = 2.5, color = "#FC4E07")
      }
      if(input$choix_box == 3){
        plot <- ggplot(data_variable(), aes(x = campagne, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by survey", sep=""),
               y = "")#+ add calculate of the mean
        #stat_summary(fun.y = mean, geom = "point",
        #             shape = 18, size = 2.5, color = "#FC4E07")
      }
      if(input$choix_box == 4){
        plot <- ggplot(data_variable(), aes(x = station, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by station", sep=""),
               y = "")#+ add calculate of the mean
        #stat_summary(fun.y = mean, geom = "point",
        #             shape = 18, size = 2.5, color = "#FC4E07")
      }
      if(input$choix_box == 5){
        plot <- ggplot(data_variable(), aes(x = saison, y = as.numeric(variable())))+
          geom_boxplot()+
          labs(title = paste("Boxplot of ", var_name," by season", sep=""),
               y = "")#+ add calculate of the mean
        #stat_summary(fun.y = mean, geom = "point",
        #             shape = 18, size = 2.5, color = "#FC4E07")
      }
      plot
    })

    output$boxplot <- renderPlot({
      if(is.null(data_analyse())){return()}
      if(is.null(input$choix_box)){return()}
      if(is.null(create_boxplot())){return()}
      create_boxplot()
    })

    ## Exporter le graphique
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("boxplot_", r$var_name, ".png", sep = "")
      },
      content = function(file) {
        # Use tryCatch to handle errors with try(silent = TRUE)
        tryCatch(
          {
            ggsave(file, plot = create_boxplot(), height = 9, width = 12,
                   bg = "white")
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
    #####

    ###### old version ######
    ## histogram ###
    #histogram <- reactive({
    #  if(is.null(data_analyse())){return()}
    #  data <- as.data.frame(variable())
    #  names(data) <- c("variable")
    #  ggplot(data, aes(x = variable))+
    #    geom_histogram(fill="lightblue", color="black", bins = 50)+
    #    labs(title = paste("Histogram of ", r$var_name, sep=""),
    #         x=r$var_name, y="Frequency")
    #})
    #output$hist <- renderPlot({
    #  if(is.null(histogram())){return()}
    #  histogram()
    #  #hist(as.numeric(variable()),
    #  #     main = paste("Histogram of ", r$var_name, sep=""),
    #  #     xlab = r$var_name, ylab = "Frequency", col = "lightblue")
    #})

    ## Exporter le graphique
    #output$downloadPlot <- downloadHandler(
    #  filename = function() {
    #    paste("histogram_", r$var_name, ".png", sep = "")
    #  },
    #  content = function(file) {
    #    # Use tryCatch to handle errors with try(silent = TRUE)
    #    tryCatch(
    #      {
    #        ggsave(file, plot = histogram(), height = 9, width = 16,
    #               bg = "white")
    #      },
    #      error = function(e) {
    #        # Handle the error here (print a message, log it, etc.)
    #        print("")
    #      },
    #      warning = function(w) {
    #        # Handle warnings if needed
    #        print("")
    #      }
    #    )
    #  })



    #output$co_saison_traitement <- renderPlot({
    #  if(is.null(data_analyse())){return()}
    #  coplot(as.numeric(data_analyse()[,1]) ~ data_analyse()$saison | data_analyse()$traitement,
    #         data = data_analyse(),
    #         main = paste("Coplot of ", r$var_name," by season and traitment", sep=""),
    #         xlab = "", ylab = "", xlim = (c(min(as.numeric(data_analyse()[,1])),
    #                                         max(as.numeric(data_analyse()[,1]))))
    #         )
    #}) pas lisible



    #### interaction plot part ###
    #output$choix_interaction <- renderUI({
    #  selectInput(
    #    ns("choix_interaction"),
    #    i18n$t("Sélectionner l'interaction :"),
    #    c("traitement_saison" = "1",
    #      "traitement_year" = "2",
    #      "traitement_campagne" = "3",
    #      "traitement_station" = "4"),
    #    selected = "1"
    #  )
    #})
#
    #output$interact <- renderPlot({
    #  if(is.null(data_analyse())){return()}
    #  if(is.null(input$choix_interaction)){return()}
    #  if(input$choix_interaction == 1){
    #    interaction.plot(x.factor=data_variable()$saison,
    #                     trace.factor=data_variable()$traitement,
    #                     trace.label = "traitment",
    #                     response=as.numeric(variable()),
    #                     main = paste("Interaction_plot of ", r$var_name," by season and traitment", sep=""),
    #                     xlab = "", ylab = "")
    #  }
    #  if(input$choix_interaction == 2){
    #    interaction.plot(x.factor=data_variable()$year,
    #                     trace.factor=data_variable()$traitement,
    #                     trace.label = "traitment",
    #                     response=as.numeric(variable()),
    #                     main = paste("Interaction_plot of ", r$var_name," by year and traitment", sep=""),
    #                     xlab = "", ylab = "")
    #  }
    #  if(input$choix_interaction == 3){
    #    interaction.plot(x.factor=data_variable()$campagne,
    #                     trace.factor=data_variable()$traitement,
    #                     trace.label = "traitment",
    #                     response=as.numeric(variable()),
    #                     main = paste("Interaction_plot of ", r$var_name," by survey and traitment", sep=""),
    #                     xlab = "", ylab = "")
    #  }
    #  if(input$choix_interaction == 4){
    #    interaction.plot(x.factor=data_variable()$station,
    #                     trace.factor=data_variable()$traitement,
    #                     trace.label = "traitment",
    #                     response=as.numeric(variable()),
    #                     main = paste("Interaction_plot of ", r$var_name," by sation and traitment", sep=""),
    #                     xlab = "", ylab = "")
    #  }
    #})

}

## To be copied in the UI
# mod_Stat_ui("Stat_1")

## To be copied in the server
# mod_Stat_server("Stat_1")
