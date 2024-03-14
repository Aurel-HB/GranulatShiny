#' tutti_function_traitement
#'
#' @description Mise en forme de la table à partir du format tutti
#' Table formatting using tutti format
#'
#' @param tutti_catch dataframe
#' @param tutti_operation dataframe
#' @param liste_station character
#' @param liste_dates dataframe
#' @param zones integer
#'
#'
#' @return The return value is a list of 2 dataframe
#'
#'

tutti_function_traitement<- function(tutti_catch, tutti_operation, liste_station, liste_dates, zones) {
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


  tutti_operation$Distance <- tutti_operation$Distance / 1000
  tutti_operation$Horizontal_opening <- 0.01
  tutti_operation$hauled_surf <-
    tutti_operation$Horizontal_opening * tutti_operation$Distance

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

  ## saison
  # Extract month and day
  tutti_catch_abun$month <- format(tutti_catch_abun$date, "%m")
  tutti_catch_abun$day <- format(tutti_catch_abun$date, "%d")

  # Convert month and day to numeric if needed
  tutti_catch_abun$month <- as.numeric(tutti_catch_abun$month)
  tutti_catch_abun$day <- as.numeric(tutti_catch_abun$day)


  tutti_catch_abun$saison <- NA

  {
    tutti_catch_abun[tutti_catch_abun$month == 1 |
                       tutti_catch_abun$month == 2, 8] <- "Winter"
    tutti_catch_abun[tutti_catch_abun$month == 3 &
                       tutti_catch_abun$day < 20,   8] <- "Winter"

    tutti_catch_abun[tutti_catch_abun$month == 3 &
                       tutti_catch_abun$day >= 20,  8] <- "Spring"
    tutti_catch_abun[tutti_catch_abun$month == 4 |
                       tutti_catch_abun$month == 5, 8] <- "Spring"
    tutti_catch_abun[tutti_catch_abun$month == 6 &
                       tutti_catch_abun$day < 21,   8] <- "Spring"

    tutti_catch_abun[tutti_catch_abun$month == 6 &
                       tutti_catch_abun$day >= 21,  8] <- "Summer"
    tutti_catch_abun[tutti_catch_abun$month == 7 |
                       tutti_catch_abun$month == 8, 8] <- "Summer"
    tutti_catch_abun[tutti_catch_abun$month == 9 &
                       tutti_catch_abun$day < 22,   8] <-  "Summer"

    tutti_catch_abun[tutti_catch_abun$month == 9 &
                       tutti_catch_abun$day >= 22,    8] <- "Autumn"
    tutti_catch_abun[tutti_catch_abun$month == 10 |
                       tutti_catch_abun$month == 11, 8] <- "Autumn"
    tutti_catch_abun[tutti_catch_abun$month == 12 &
                       tutti_catch_abun$day < 21,    8] <- "Autumn"

    tutti_catch_abun[tutti_catch_abun$month == 12 &
                       tutti_catch_abun$day >= 21, 8] <- "Winter"
  }

  tutti_catch_abun <-
    tutti_catch_abun %>% select(-c("month", "day"))


  ## Survey and tow
  tutti_catch_abun <-
    tutti_catch_abun %>% left_join(tutti_operation[, c(2:5, 12)], by = c("station", "date"))



  tutti_catch_abun_wide <-
    pivot_wider(tutti_catch_abun, #tutti_catch_abun[,-4] pour faire en relatif
                names_from = "species",
                values_from = "abun") # abun_km2 pour faire en relatif
  ## traitement
  traitement<-NA
  traitement<-rep("Sans impact", nrow(tutti_catch_abun_wide))
  tutti_catch_abun_wide$traitement<-traitement_maker_zones(tutti_catch_abun_wide, liste_station, liste_dates, zones, traitement=traitement)
  tutti_catch_abun_wide <- tutti_catch_abun_wide %>% relocate(1:7, traitement)

  ## variable interaction
  tutti_catch_abun_wide$interaction<-interaction(tutti_catch_abun_wide$traitement, tutti_catch_abun_wide$saison, sep=":")
  tutti_catch_abun_wide <- tutti_catch_abun_wide %>% relocate(1:8, interaction)

  # catch table shapping - BIOMASS

  tutti_catch_biom <-
    tutti_catch %>% select("Annee", "Code_Station", "Nom_Scientifique", "Poids", "DateDeb")

  {
    names(tutti_catch_biom)[1] <- "year"
    names(tutti_catch_biom)[2] <- "station"
    names(tutti_catch_biom)[3] <- "species"
    names(tutti_catch_biom)[4] <- "biom"
    names(tutti_catch_biom)[5] <- "date"
    }

  ## saison
  # Extract month and day
  tutti_catch_biom$month <- format(tutti_catch_biom$date, "%m")
  tutti_catch_biom$day <- format(tutti_catch_biom$date, "%d")

  # Convert month and day to numeric if needed
  tutti_catch_biom$month <- as.numeric(tutti_catch_biom$month)
  tutti_catch_biom$day <- as.numeric(tutti_catch_biom$day)

  ## saison

  tutti_catch_biom$saison <- NA

  {
    tutti_catch_biom[tutti_catch_biom$month == 1 |
                       tutti_catch_biom$month == 2, 8] <- "Winter"
    tutti_catch_biom[tutti_catch_biom$month == 3 &
                       tutti_catch_biom$day < 20,   8] <- "Winter"

    tutti_catch_biom[tutti_catch_biom$month == 3 &
                       tutti_catch_biom$day >= 20,  8] <- "Spring"
    tutti_catch_biom[tutti_catch_biom$month == 4 |
                       tutti_catch_biom$month == 5, 8] <- "Spring"
    tutti_catch_biom[tutti_catch_biom$month == 6 &
                       tutti_catch_biom$day < 21,   8] <- "Spring"

    tutti_catch_biom[tutti_catch_biom$month == 6 &
                       tutti_catch_biom$day >= 21,  8] <- "Summer"
    tutti_catch_biom[tutti_catch_biom$month == 7 |
                       tutti_catch_biom$month == 8, 8] <- "Summer"
    tutti_catch_biom[tutti_catch_biom$month == 9 &
                       tutti_catch_biom$day < 22,   8] <-  "Summer"

    tutti_catch_biom[tutti_catch_biom$month == 9 &
                       tutti_catch_biom$day >= 22,    8] <- "Autumn"
    tutti_catch_biom[tutti_catch_biom$month == 10 |
                       tutti_catch_biom$month == 11, 8] <- "Autumn"
    tutti_catch_biom[tutti_catch_biom$month == 12 &
                       tutti_catch_biom$day < 21,    8] <- "Autumn"

    tutti_catch_biom[tutti_catch_biom$month == 12 &
                       tutti_catch_biom$day >= 21, 8] <- "Winter"
  }

  tutti_catch_biom <-
    tutti_catch_biom %>% select(-c("month", "day"))
  tutti_catch_biom$date <-
    as.Date(tutti_catch_biom$date, format = "%d/%m/%Y %H:%M")


  ## Survey and tow

  tutti_catch_biom <-
    tutti_catch_biom %>% left_join(tutti_operation[, c(2:5, 12)], by = c("station", "date"))


  tutti_catch_biom_wide <-
    pivot_wider(tutti_catch_biom, #tutti_catch_biom[,-4] en relatif
                names_from = "species",
                values_from = "biom") # biom_km2 pour faire en relatif

  ## traitement
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
