#' pre_leaflet
#'
#' @description Formate un df pour la visualisation des traits de chalut et des labels associés à ces traits
#' prepare a dataframe to visualize the trawl haul with associated labels
#'
#' @param data dataframe
#' @param liste_station character
#' @param liste_dates dataframe
#' @param zones number
#'
#'
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
pre_leaflet <- function(data, liste_station, liste_dates, zones) {
  data$impact_date_1<-NA
  data$impact_date_2<-NA
  data$zones<-NA
  for (y in 1:as.numeric(zones)) {
    for (i in 1:nrow((data))) {
      if (data$Code_Station[i] %in% liste_station[[y]]) {
        data$impact_date_1[i] <- as.character(liste_dates[[y]][1])
        data$impact_date_2[i] <- as.character(liste_dates[[y]][2])
        data$zones[i]<-y
      }
    }
  }
  data
}
