#' Representation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Representation_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    # Inputs pour parametrer le graph
    sidebarPanel(
      h4(strong(i18n$t("Formulation du modèle :"))),
      htmlOutput(ns("ecriture_modele")),
      hr(),
      #choix du terme
      box(
        title = "Predictor",
        solidHeader = TRUE,
        status = "danger",
        uiOutput(ns("pred_1")),
        uiOutput(ns("pred_2")),
        width = "100%"
        ),
      box(
        title = i18n$t("Covariable"),
        solidHeader = TRUE,
        status = "success",
        color = "lime",
        uiOutput(ns("cov_1")),
        uiOutput(ns("cov_2")),
        uiOutput(ns("cov_3")),
        #uiOutput(ns("effet_cov")),
        width = "100%"
      ),
      #afficher ou non les lettres post-hocs
      #checkboxInput(ns("post_hoc"), "Afficher les lettres de significativitées"),
      # gerer la place des lettres
      #uiOutput(ns("largeur_plot")),
      #uiOutput(ns("hauteur_plot")),
      #telecharger le graphique
      downloadButton(ns("downloadPlot_effet"), label = i18n$t("Telecharger le graphique (.png)"))
    ),
    #Output du grah
    mainPanel( box(
      solidHeader = F,
      status = "primary",
      width = "100%",
      plotOutput(ns("effet_plot")) #%>% withSpinner(color ="#00619a")
    ),
      verbatimTextOutput(ns("test1")),
      verbatimTextOutput(ns("test2"))
              )
  )
}

#' Representation Server Functions
#'
#' @noRd
mod_Representation_server <- function(input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns

    # import of the data
    modele <- reactive({
      if (is.null(r$modele)){return()}
      if(class(r$modele) == "try-error"){return()}
      r$modele
    })


    output$ecriture_modele <- renderText(r$ecriture[[1]])

    # Render UI pour la visualisation
    output$pred_1 <-
      renderUI({
        if(is.null(modele())){return()}
        selectInput(
          ns("pred_1"),
          i18n$t("Choississez un prédicteur"),
          choices = c("traitement", "saison", r$covariable),
          multiple = F,
          selected = c("traitement")  # You can set default selected values if needed
        )
      })

    output$pred_2 <-
      renderUI({
        if(is.null(modele())){return()}
        choix <- c("traitement", "saison", r$covariable)
        selectInput(
          ns("pred_2"),
          i18n$t("Choississez un second prédicteur"),
          choices = choix[!(choix %in% input$pred_1)],
          multiple = F,
          selected = c("saison")  # You can set default selected values if needed
        )
      })

    output$cov_1 <- # prepare work of the first covariable
      renderUI({
        if(is.null(modele())){return()}
        if(is.null(r$covariable)){return()}
        selectInput(
          ns("cov_1"),
          paste(i18n$t("Fixez une valeur de la covariable "), r$covariable[1], sep = ""),
          choices = r$data_analyse[, r$covariable[1]],
          multiple = F
        )
      })

    output$cov_2 <- # prepare work of the first covariable
    renderUI({
      if(is.null(modele())){return()}
      if(is.null(r$covariable)){return()}
      if(length(r$covariable) < 2){return()}
      selectInput(
        ns("cov_2"),
        paste(i18n$t("Fixez une valeur de la covariable "), r$covariable[2], sep = ""),
        choices = r$data_analyse[, r$covariable[2]],
        multiple = F
      )
    })

    output$cov_3 <- # prepare work of the first covariable
      renderUI({
        if(is.null(modele())){return()}
        if(is.null(r$covariable)){return()}
        if(length(r$covariable) < 3){return()}
        selectInput(
          ns("cov_3"),
          paste(i18n$t("Fixez une valeur de la covariable "), r$covariable[3], sep = ""),
          choices = r$data_analyse[, r$covariable[3]],
          multiple = F
        )
      })


    #even if the cov window is null if you pass from 3 cov to 2 cov
    #so we will use an interposed variable because it is not possible to change
    #directly an input
    cov_1 <- reactive({
      if(is.null(input$cov_1)){return()}
      else if(input$cov_1 == ""){return()}
      input$cov_1
    })

    cov_2 <- reactive({
      if(is.null(input$cov_2)){return()}
      if(length(r$covariable) < 2){return()}
      else if(input$cov_2 == ""){return()}
      input$cov_2
    })

    cov_3 <- reactive({
      if(is.null(input$cov_3 )){return()}
      if(length(r$covariable) < 3){return()}
      else if(input$cov_3 == ""){return()}
      input$cov_3
    })

    #output$effet_cov <-renderUI({
    #  if(is.null(cov_1())){return()}
    #  else if(length(r$covariable) == 2 & is.null(cov_2())){return()}
    #  else if(length(r$covariable) == 3 & is.null(cov_3())){return()}
    #  return(
    #    checkboxInput(ns("effet_cov"), "Appliquer les effets des covariables")
    #  )
    #})


    # plot_model : fonction pour representer graphiquement les effets marginaux####
    plot_effet <- reactive({
      if(is.null(modele())){return()}
      if(is.null(input$pred_1)){return()}
      if(is.null(input$pred_2)){return()}
      if(input$pred_1 == input$pred_2){return()}
      try(
        ggpredict(modele()[[2]], terms = c(input$pred_1, input$pred_2))|> plot(),
        silent = T)
    })

    # To use ggpredict the covariable information need to be in the format Named num
    plot_effet_covariable <- reactive({
      if(is.null(modele())){return()}
      if(is.null(r$covariable)){return()}
      else if(is.null(cov_1())){return()}
      else if(is.null(cov_2()) && length(r$covariable) < 2){
        condition <- c(cov_1())
        names(condition) <- c(r$covariable[1])
        return(
          ggpredict(modele()[[2]], terms = c(input$pred_1, input$pred_2),
                    condition = condition)|> plot()
        )
      }
      else if(is.null(cov_2())){return()}
      else if(is.null(cov_3()) && length(r$covariable) < 3){
        condition <- c(cov_1(), cov_2())
        names(condition) <- c(r$covariable[1],r$covariable[2])
        return(
          ggpredict(modele()[[2]], terms = c(input$pred_1, input$pred_2),
                    condition = condition)|> plot()
        )
      }
      else if(is.null(cov_3())){return()}
      else {
        condition <- c(cov_1(), cov_2(), cov_3())
        names(condition) <- c(r$covariable[1],r$covariable[2], r$covariable[3])
        return(
          ggpredict(modele()[[2]], terms = c(input$pred_1, input$pred_2),
                    condition = condition)|> plot()
        )
      }
    })

    ##### This writing doesn't work with the function ggpredict
    # ggpredict don't understand the command r$covariable[1]
    # or any value from a vector
    #else if(length(r$covariable) == 1){
    #  ggpredict(modele()[[2]], terms = c(input$pred_1, input$pred_2),
    #            condition = c(r$covariable[1] = input$cov_1))|> plot()
    #  }
    #####

    ################
    # chexkpoint ####
    #output$test1 <- renderPrint({
    #  class(input$cov_1)
    #})
    #####

    # chexkpoint ####
    #output$test2 <- renderPrint({
    #  list(r$covariable,cov_1(), cov_3(), class(input$effet_cov))
    #})
    #####

    ##test post-hoc entre les modalités
    #lettre <- reactive({
    #  if (input$post_hoc == T) {
    #    data_group <-
    #      as.data.frame(cld(
    #        lsmeans(modele()[[2]], effet_choix()),
    #        alpha = 0.05,
    #        Letters = letters,
    #        adjust = "fdr"
    #      ))
    #    if (length(effet_choix()) == 2) {
    #      lettre_group <-
    #        data_group[order(data_group$traitement, data_group$saison),]$.group
    #    } else {
    #      lettre_group <- data_group$.group
    #    }
    #  } else {
    #    lettre_group <- ""
    #  }
    #})

    #output$largeur_plot <- renderUI({
    #  if(input$post_hoc == F){return()}
    #  numericInput(
    #    ns("largeur_plot"),
    #    "Régler la largeur des lettres",
    #    value = NULL,
    #    step = 0.1
    #  )
    #})
#
    #output$hauteur_plot <- renderUI({
    #  if(input$post_hoc == F){return()}
    #  numericInput(
    #    ns("hauteur_plot"),
    #    "Régler la hauteur des lettres",
    #    value = NULL,
    #    step = 0.1
    #  )
    #})


    ##Plot des effets avec lettre post hoc
    #plot_effet_final <- reactive({
    #  if (length(effet_choix()) == 2) {
    #    plot_effet_int() +
    #      geom_text(
    #        aes(label = lettre()),
    #        hjust = input$largeur_plot,
    #        vjust = input$hauteur_plot,
    #        size = 5,
    #        check_overlap = T,
    #        col = "black"
    #      ) +
    #      ggtitle("") +
    #      theme_classic()
    #  } else {
    #    plot_effet()[effet_choix()][[1]] +
    #      ggtitle("") +
    #      theme_classic() +
    #      geom_text(
    #        aes(label = lettre()),
    #        size = 5,
    #        check_overlap = T,
    #        hjust = input$largeur_plot,
    #        vjust = input$hauteur_plot,
    #        col = "black"
    #      )

    #  }
    #})

    plot_final <- reactive({
      if (is.null(plot_effet())){return(NULL)}
      else if (is.null(r$covariable)){return(plot_effet())}
      else{return(plot_effet_covariable())}
    })

    output$effet_plot <- renderPlot({
      plot_final()
    })



    ## Exporter le graphique
      output$downloadPlot_effet <- downloadHandler(
        filename = function() {
          paste("plot_marginal", ".png", sep = "")
        },
        content = function(file) {
          # Use tryCatch to handle errors with try(silent = TRUE)
          tryCatch(
            {
              ggsave(file, plot = plot_final(), height = 4.72, width = 8.75, bg = "white")
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

}#
#
## To be copied in the UI
# mod_Representation_ui("Representation_1")
#
## To be copied in the server
# mod_Representation_server("Representation_1")
