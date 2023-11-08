#' tutti_function_traitement
#'
#' @description Mise en forme de la table à partir du format tutti
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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

  tutti_operation$date <-
    as.Date(tutti_operation$date, format = "%d/%m/%Y %H:%M")

  tutti_operation$Distance <- tutti_operation$Distance / 1000
  tutti_operation$Horizontal_opening <- 0.01
  tutti_operation$hauled_surf <-
    tutti_operation$Horizontal_opening * tutti_operation$Distance

  # variable campagne
  campagne<-c(1)
  for( i in 2:nrow(tutti_operation)){
    if (tutti_operation$tow[i]>tutti_operation$tow[i-1]){
      campagne[i] <- campagne[i-1] } else {
        campagne[i]<- campagne[i-1]+1
      }
  }

  tutti_operation$campagne<-campagne

  # catch table shapping - ABUNDANCE

  tutti_catch_abun <-
    tutti_catch %>% select("Annee", "Trait", "Nom_Scientifique", "Nombre", "DateDeb")

  {
    names(tutti_catch_abun)[1] <- "year"
    names(tutti_catch_abun)[2] <- "station"
    names(tutti_catch_abun)[3] <- "species"
    names(tutti_catch_abun)[4] <- "abun"
    names(tutti_catch_abun)[5] <- "date"
    }

  ## saison
  tutti_catch_abun$month <-
    as.numeric(substr(as.character(tutti_catch_abun$date), 4, 5))
  tutti_catch_abun$day <-
    as.numeric(substr(as.character(tutti_catch_abun$date), 1, 2))

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
  tutti_catch_abun$date <-
    as.Date(tutti_catch_abun$date, format = "%d/%m/%Y %H:%M")


  ## Survey and tow
  tutti_catch_abun <-
    tutti_catch_abun %>% left_join(tutti_operation[, c(2:5, 12)], by = c("station", "date"))



  # Abun per km2 in each tow
  # tutti_catch_abun$abun_km2 <-
  #   round(tutti_catch_abun$abun / tutti_catch_abun$hauled_surf,
  #         digits = 0)

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
    tutti_catch %>% select("Annee", "Trait", "Nom_Scientifique", "Poids", "DateDeb")

  {
    names(tutti_catch_biom)[1] <- "year"
    names(tutti_catch_biom)[2] <- "station"
    names(tutti_catch_biom)[3] <- "species"
    names(tutti_catch_biom)[4] <- "biom"
    names(tutti_catch_biom)[5] <- "date"
    }

  ## saison
  tutti_catch_biom$month <-
    as.numeric(substr(as.character(tutti_catch_biom$date), 4, 5))
  tutti_catch_biom$day <-
    as.numeric(substr(as.character(tutti_catch_biom$date), 1, 2))

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



  # tutti_catch_biom$biom_km2 <-
  #   round(tutti_catch_biom$biom / tutti_catch_biom$hauled_surf,
  #         digits = 3)

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
  colnames(tutti_catch_abun_wide)<-str_replace(colnames(tutti_catch_abun_wide), " ", "_")
  colnames(tutti_catch_biom_wide)<-str_replace(colnames(tutti_catch_biom_wide), " ", "_")
  return(list(tutti_catch_abun_wide, tutti_catch_biom_wide))
}
