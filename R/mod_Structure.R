#' Structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import vegan
#' @import ggplot2
#'
mod_Structure_ui <- function(id){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
  ns <- NS(id)
  tagList(
    #verbatimTextOutput(ns("test")),
    box(
      title =  actionButton(ns("info"), "",icon = icon("circle-info")),
      hr(),
      dataTableOutput(ns("percent")),
      downloadButton(ns("downloadData"),
                     label = i18n$t("Telecharger le tableau (.csv)")),
      width = NULL,
      style = "overflow-x: scroll;",
      collapsible = T,
      solidHeader = TRUE,
      status = "primary"
    ),
    box(
      uiOutput(ns("choix_campagne")),
      actionButton(ns("info2"), "",icon = icon("circle-info")),
      actionButton(ns("info3"), "",icon = icon("circle-info")),
      actionButton(ns("info4"), "",icon = icon("circle-info")),
      plotOutput(ns("plot"), width = "100%"),
      #telecharger le graphique
      downloadButton(ns("downloadPlot"),
                     label = i18n$t("Telecharger le graphique (.png)")),
      width = NULL,
      style = "overflow-x: scroll;",
      collapsible = T,
      collapsed = T,
      solidHeader = TRUE,
      status = "info"
    ),
    actionButton("descript", i18n$t("Statistiques descriptives"), icon = icon("ship"))
  )
}

#' Structure Server Functions
#'
#' @noRd
mod_Structure_server <- function (input, output, session, r){
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fr")
    ns <- session$ns
    dataset <- reactive({
      if(is.null(r$data_forme)){return()}
      if(is.null(r$data_form_modif)){
        return(r$data_forme[[1]])
      } else {
        return(r$data_form_modif)
      }
      })
    species <- reactive({r$species})


    data_brut <- reactive ({
      if(is.null(r$data_forme)){return()}
      if(is.null(r$ID_campagne)){return()}
      data <- data.frame(r$ID_campagne)
      for (sp in species()){
        data <- cbind(data, structure_table(dataset(), sp))
      }
      position <- grep("tot_value", names(data))
      data <- data[,c(position)] #as.data.frame(data[,c(1,5,8)])
      tot <- rowSums(data)
      data <- cbind(r$ID_campagne, data, tot)
      names(data) <- c("ID_campagne",species(),"Total")
      data
    })

    # test ###
    #output$test <- renderPrint({
    #  list(class(Barplot()), Barplot())
    #})

    data_percent <- reactive({
      if(is.null(data_brut())){return()}
      data <- data_brut()
      for (i in 1:nrow(data)){
        for (sp in 1:length(species())){
          data[i,sp+1] <- round(data[i,sp+1]/data$Total[i], digits = 2)
        }
      }
      data_t <- t(data[,2:(length(species())+1)])
      #redefine row and column names
      colnames(data_t) <- r$ID_campagne
      data_t <- as.data.frame(cbind(species(),data_t))
      names(data_t)[1] <- c("species")
      data_t
    })

    output$percent <- renderDataTable({
      data_percent()
    })

    # Donwload data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data_structure", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_percent(),  file)
      }
    )

    output$choix_campagne <- renderUI({
      selectInput(
        ns("choix_campagne"),
        i18n$t("Sélectionner la campagne :"),
        r$ID_campagne,
        selected = r$ID_campagne[1]
      )
    })

    plot <- reactive({
      if(is.null(data_brut())){return()}
      if(is.null(input$choix_campagne)){return()}
      #### prepare data ####
      data_t <- as.data.frame(t(data_brut()[,2:(length(species())+1)]))
      #rownames(data_t) <- species()
      colnames(data_t) <- r$ID_campagne
      data_t <- cbind(species(),data_t)
      names(data_t) <- c("species", names(data_t)[2:length(names(data_t))])

      # order the data to have the most present species
      data <- as.data.frame(data_t[c("species",input$choix_campagne)])
      names(data) <- c("species", "Abundance")
      #data <- data %>% dplyr::arrange(desc(Abundance))
      data <- data[order(data$Abundance, decreasing = TRUE), ]
      data$species <- factor(data$species,data$species)

      #### barplot ####
      # Default bar plot
      abun_tot <- sum(data$Abundance)
      barplot <- ggplot(data %>%
                          dplyr::filter(Abundance > 0.01*abun_tot),
                        aes(x=species, y=Abundance, fill=species)) +
        geom_bar(stat="identity", color="black",
                 position=position_dodge())+
      # Finished bar plot
      labs(title=paste("Abundance per species for the survey ",
                         input$choix_campagne, sep=""), fill = NULL)+
        theme_classic() +
        # Reduce the size of the plot
        theme(
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.margin = margin(t = 0, unit = "cm"), # Adjust margin if needed
          legend.text=element_text(size=8))+ # Adjust legend text size if needed
        scale_fill_viridis_d()
      # change the display of species for too many species
      if (length(data$species) > 5){
        # Set the threshold for displaying species names
        threshold <- 5  # Adjust this value based on your preference

        # Get the species names and filter them based on the threshold
        species_names <- data$species
        filtered_species_names <-
          ifelse(seq_along(species_names) %% threshold == 0, species_names, "")

        # Update x-axis labels with filtered species names
        barplot <- barplot + scale_x_discrete(labels = filtered_species_names)
      }

      #### cumulative plot ####
      # Create a cumulative sum of abundances
      data$cumulative_abundance <- cumsum(data$Abundance)

      # Create the cumulative abundance curve with number of species
      # on the x-axis using ggplot2 and geom_step
      cumul_plot <- ggplot(data, aes(x = seq_along(cumulative_abundance),
                                          y = cumulative_abundance)) +
        geom_step() +
        labs(title = "Cumulative Abundance Curve",
             x = "Number of Species",
             y = "Cumulative Abundance")

      #### Species accumulation curves ####
      indice_campagne <- as.integer(substr(input$choix_campagne,
                                           start = 2, stop = 2))
      SAC <- specaccum(dataset() %>%
                         dplyr::filter(campagne==indice_campagne) %>%
                         dplyr::select(species()), "random")
      table_SAC <- data.frame(site = SAC$sites, richness = SAC$richness,
                              sd = SAC$sd)# table_SAC export the information
      #from the list generate by speccacum
      SAC_plot <- ggplot(table_SAC, aes(site))+
        geom_ribbon(aes(ymin = richness - sd,
                        ymax = richness + sd), fill = "lightblue")+
        geom_line(aes(y=richness), color="blue")+
        #geom_errorbar(aes(ymin=richness-sd, ymax=richness+sd), width=.2,
        #              position=position_dodge(0.5)) +
        labs(title = "Species accumulation curves",
             x = "Number of Sites",
             y = "Number of Species")

      ### prepare the plot to be display ####
      p <- plot_grid(cumul_plot, SAC_plot, ncol = 1, rel_heights = c(5,5))

      return(plot_grid(barplot,p, ncol = 2, rel_widths = c(5,4)))
    })

    output$plot <- renderPlot({
      plot()
    })

    ## Exporter le graphique
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot_structure_", input$choix_campagne, ".png", sep = "")
      },
      content = function(file) {
        # Use tryCatch to handle errors with try(silent = TRUE)
        tryCatch(
          {
            ggsave(file, plot = plot(), height = 9, width = 16, bg = "white")
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

    ##### information #####
    observeEvent(input$info,{
      message <- as.character(list_translate[r$lang][4,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info2,{
      message <- as.character(list_translate[r$lang][5,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info3,{
      message <- as.character(list_translate[r$lang][6,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })

    observeEvent(input$info4,{
      message <- as.character(list_translate[r$lang][7,1])
      sendSweetAlert(
        session = session,
        title = "",
        text = message,
        type = "info"
      )
    })
    ######
}

## To be copied in the UI
# mod_Structure_ui("Structure_1")

## To be copied in the server
# mod_Structure_server("Structure_1")
