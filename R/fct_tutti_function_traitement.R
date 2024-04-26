#' tutti_function_traitement
#'
#' @description Table formatting using GranulatShiny format
#'use dplyr and tidyr
#'
#' @param tutti_catch dataframe
#' @param tutti_operation dataframe
#' @param liste_station character
#' @param liste_dates dataframe
#' @param zones integer
#' @param trawl_opening numeric
#'
#'
#' @return The return value is a list of 2 dataframe
#'
#'

tutti_function_traitement<- function(tutti_catch, tutti_operation, liste_station,
                                     liste_dates, zones, trawl_opening) {
  # operation table shapping



  tutti_operation <-
    tutti_operation %>% select(
      "Annee",
      "Serie_Partielle",
      "Code_Station",
      "Id_Operation",
      "DateDeb",
      "LatDeb",
      "LatFin",
      "LongDeb",
      "LongFin",
      "Distance"
    )

  {
    names(tutti_operation)[1] <- "year"
    names(tutti_operation)[2] <- "campagne"
    names(tutti_operation)[3] <- "station"
    names(tutti_operation)[4] <- "tow"
    names(tutti_operation)[5] <- "date"
    }


  tutti_operation$Distance <- tutti_operation$Distance/1000 # to pass in km
  tutti_operation$Horizontal_opening <- trawl_opening/1000
  tutti_operation$hauled_surf <-
    round(tutti_operation$Horizontal_opening * tutti_operation$Distance,
          digits = 6)

  # variable campagne calculate by the combination of Annee and Serie_Partielle
  survey_Date <- sort(unique(tutti_operation$year))
  survey_ID <- unique(tutti_operation$campagne)

  for(i_tutti in 1:nrow(tutti_operation)){
    for (i_ref in 1:length(survey_Date)){
      if (tutti_operation$year[i_tutti] == survey_Date[i_ref]){
        tutti_operation$campagne[i_tutti] <- tutti_operation$campagne[i_tutti] + length(survey_ID)*(i_ref-1)
      }
    }
  }

  # in the specific case there is a gap in the survey (2022_serie1 to 2023_serie2)
  real_nb_survey <- seq(1,length(unique(tutti_operation$campagne)),1)
  tutti_campagne <- sort(unique(tutti_operation$campagne))
  for( i in 1:length(real_nb_survey)){
    if(real_nb_survey[i] != tutti_campagne[i]){
      tutti_operation <-
        tutti_operation %>% dplyr::mutate(campagne = ifelse(campagne == tutti_campagne[i],
                                                            real_nb_survey[i],
                                                            campagne))
    }
  }


  # catch table shapping - ABUNDANCE

  tutti_catch_abun <-
    tutti_catch %>% select("Annee", "Code_Station", "Nom_Scientifique", "Nombre", "DateDeb")

  {
    names(tutti_catch_abun)[1] <- "year"
    names(tutti_catch_abun)[2] <- "station"
    names(tutti_catch_abun)[3] <- "species"
    names(tutti_catch_abun)[4] <- "abun"
    names(tutti_catch_abun)[5] <- "date"
    }

  # this version create the survey indice also in the catch table to compile the 2 table with this variable####
  #tutti_catch_abun <-
  #  tutti_catch %>% select("Annee","Serie_Partielle", "Code_Station", "Nom_Scientifique", "Nombre", "DateDeb")
#
  #{
  #  names(tutti_catch_abun)[1] <- "year"
  #  names(tutti_catch_abun)[2] <- "campagne"
  #  names(tutti_catch_abun)[3] <- "station"
  #  names(tutti_catch_abun)[4] <- "species"
  #  names(tutti_catch_abun)[5] <- "abun"
  #  names(tutti_catch_abun)[6] <- "date"
  #  }

  ## variable campagne calculate by the combination of Annee and Serie_Partielle
  #survey_Date <- sort(unique(tutti_catch_abun$year))
  #survey_ID <- unique(tutti_catch_abun$campagne)
  #
  #for(i_tutti in 1:nrow(tutti_catch_abun)){
  #  for (i_ref in 1:length(survey_Date)){
  #    if (tutti_catch_abun$year[i_tutti] == survey_Date[i_ref]){
  #      tutti_catch_abun$campagne[i_tutti] <- tutti_catch_abun$campagne[i_tutti] + length(survey_ID)*(i_ref-1)
  #    }
  #  }
  #}
  #
  ## in the specific case there is a gap in the survey (2022_serie1 to 2023_serie2)
  #real_nb_survey <- seq(1,length(unique(tutti_catch_abun$campagne)),1)
  #tutti_campagne <- sort(unique(tutti_catch_abun$campagne))
  #for( i in 1:length(real_nb_survey)){
  #  if(real_nb_survey[i] != tutti_campagne[i]){
  #    tutti_catch_abun <-
  #      tutti_catch_abun %>% dplyr::mutate(campagne = ifelse(campagne == tutti_campagne[i],
  #                                                          real_nb_survey[i],
  #                                                          campagne))
  #  }
  #}
  #####

  ## Survey and tow
  tutti_catch_abun <-
    tutti_catch_abun %>% left_join(tutti_operation[, c(2:5, 12)], by = c("station", "date"))

  # Transform number of fishes in density
  tutti_catch_abun$abun <- round(tutti_catch_abun$abun / tutti_catch_abun$hauled_surf,
                                 digits = 0)


  tutti_catch_abun_wide <-
    pivot_wider(tutti_catch_abun,
                names_from = "species",
                values_from = "abun") # reduce nb of rows and increase nb columns
  # the species in line become columns and a row is a station/date

  # add the station where there no data ####
  # transformation of the tutti_operation as the tutti_catch_abun_wide
  ## saison
  # Extract month and day
  tutti_operation$month <- format.Date(tutti_operation$date, "%m")
  tutti_operation$day <- format.Date(tutti_operation$date, "%d")

  # Convert month and day to numeric if needed
  tutti_operation$month <- as.numeric(tutti_operation$month)
  tutti_operation$day <- as.numeric(tutti_operation$day)
  tutti_operation$saison <- NA
  col_saison <- length(names(tutti_operation))

  {
    tutti_operation[tutti_operation$month == 1 |
                       tutti_operation$month == 2, col_saison] <- "Winter"
    tutti_operation[tutti_operation$month == 3 &
                       tutti_operation$day < 20,   col_saison] <- "Winter"

    tutti_operation[tutti_operation$month == 3 &
                       tutti_operation$day >= 20,  col_saison] <- "Spring"
    tutti_operation[tutti_operation$month == 4 |
                       tutti_operation$month == 5, col_saison] <- "Spring"
    tutti_operation[tutti_operation$month == 6 &
                       tutti_operation$day < 21,   col_saison] <- "Spring"

    tutti_operation[tutti_operation$month == 6 &
                       tutti_operation$day >= 21,  col_saison] <- "Summer"
    tutti_operation[tutti_operation$month == 7 |
                       tutti_operation$month == 8, col_saison] <- "Summer"
    tutti_operation[tutti_operation$month == 9 &
                       tutti_operation$day < 22,   col_saison] <-  "Summer"

    tutti_operation[tutti_operation$month == 9 &
                       tutti_operation$day >= 22,    col_saison] <- "Autumn"
    tutti_operation[tutti_operation$month == 10 |
                       tutti_operation$month == 11, col_saison] <- "Autumn"
    tutti_operation[tutti_operation$month == 12 &
                       tutti_operation$day < 21,    col_saison] <- "Autumn"

    tutti_operation[tutti_operation$month == 12 &
                       tutti_operation$day >= 21, col_saison] <- "Winter"
  }

  tutti_operation <-
    tutti_operation %>% select(-c("month", "day"))

  tutti_operation <-
    tutti_operation %>% select(
      "year",
      "station",
      "date",
      "saison",
      "campagne",
      "tow",
      "hauled_surf"
    ) # at this moment we have create a a tutti_operation with the same format
  # as tutti_catch_abun_wide but with all the operations

  species <- sort(unique(tutti_catch$Nom_Scientifique))
  # add a column per species
  for (indice in species){
    tutti_operation[indice] <- NA
  }

  #now completion of this column by the species catch
  for(ligne in 1:nrow(tutti_catch_abun_wide)){
    for (ind_species in species) {
          tutti_operation[tutti_operation$campagne == tutti_catch_abun_wide$campagne[ligne] &
                      tutti_operation$station == tutti_catch_abun_wide$station[ligne],
                    ind_species] <- tutti_catch_abun_wide[ind_species][[ligne,1]]
    }
  }

  tutti_catch_abun_wide <- tutti_operation

  ## traitement ####
  traitement<-NA
  traitement<-rep("Sans impact", nrow(tutti_catch_abun_wide))
  tutti_catch_abun_wide$traitement<-traitement_maker_zones(tutti_catch_abun_wide, liste_station, liste_dates, zones, traitement=traitement)
  tutti_catch_abun_wide <- tutti_catch_abun_wide %>% relocate(1:7, traitement)

  ## variable interaction
  tutti_catch_abun_wide$interaction <- interaction(tutti_catch_abun_wide$traitement, tutti_catch_abun_wide$saison, sep=":")
  tutti_catch_abun_wide <- tutti_catch_abun_wide %>% relocate(1:8, interaction)

  # catch table shapping - BIOMASS ####

  tutti_catch_biom <-
    tutti_catch %>% select("Annee", "Code_Station", "Nom_Scientifique", "Poids", "DateDeb")

  {
    names(tutti_catch_biom)[1] <- "year"
    names(tutti_catch_biom)[2] <- "station"
    names(tutti_catch_biom)[3] <- "species"
    names(tutti_catch_biom)[4] <- "biom"
    names(tutti_catch_biom)[5] <- "date"
    }


  ## Survey and tow

  tutti_catch_biom <-
    tutti_catch_biom %>% left_join(tutti_operation[, c(2:7)], by = c("station", "date"))

  # Transform biomass in biomass (kg/m^2)
  tutti_catch_biom$biom <- round(tutti_catch_biom$biom / tutti_catch_biom$hauled_surf,
                                 digits = 3)


  tutti_catch_biom_wide <-
    pivot_wider(tutti_catch_biom, #tutti_catch_biom[,-4] en relatif
                names_from = "species",
                values_from = "biom") # biom_km2 pour faire en relatif

  # add the station where there no data ####
  # add a column per species
  for (indice in species){
    tutti_operation[indice] <- NA
  }

  #now completion of this column by the species catch
  for(ligne in 1:nrow(tutti_catch_biom_wide)){
    for (ind_species in species) {
      tutti_operation[tutti_operation$campagne == tutti_catch_biom_wide$campagne[ligne] &
                        tutti_operation$station == tutti_catch_biom_wide$station[ligne],
                      ind_species] <- tutti_catch_biom_wide[ind_species][[ligne,1]]
    }
  }

  tutti_catch_biom_wide <- tutti_operation

  ## traitement ####
  traitement<-NA
  traitement<-rep("Sans impact", nrow(tutti_catch_biom_wide))
  tutti_catch_biom_wide$traitement<-traitement_maker_zones(tutti_catch_biom_wide, liste_station, liste_dates, zones, traitement=traitement)
  tutti_catch_biom_wide <- tutti_catch_biom_wide %>% relocate(1:7, traitement)
  tutti_catch_biom_wide <- tutti_catch_biom_wide %>% relocate(1:7, traitement)

  ## variable interaction
  tutti_catch_biom_wide$interaction<-interaction(tutti_catch_biom_wide$traitement, tutti_catch_biom_wide$saison, sep=":")
  tutti_catch_biom_wide <- tutti_catch_biom_wide %>% relocate(1:8, interaction)
  ## Virer les espaces
  colnames(tutti_catch_abun_wide)<-str_replace(colnames(tutti_catch_abun_wide), " ", ".")
  colnames(tutti_catch_biom_wide)<-str_replace(colnames(tutti_catch_biom_wide), " ", ".")
  return(list(tutti_catch_abun_wide, tutti_catch_biom_wide))
}
