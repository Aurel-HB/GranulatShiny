#' traitement_maker
#'
#' @description Creer la variable traitement en fonction des imformations rentrées (stations et dates)
#' à adapter pour la version "complexe"
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

traitement_maker <- function(data, station, dates, traitement) {
  # création de la variable traitement
  for (i in 1:nrow(data)) {
    if ((data$date[i]  >= dates[1]) &
        (data$date[i] <= dates[2]) &
        (data$station[i] %in% c(station)))
    {
      traitement[i] <- "Impact"
    }
    if ((data$date[i] > dates[2]) &
        data$station[i] %in% c(station))
    {
      traitement[i] <- "Recolonisation"
    }
  }
  traitement
}
