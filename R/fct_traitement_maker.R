#' traitement_maker
#'
#' @description Creer la variable traitement en fonction des imformations
#' rentrées (stations et dates)
#' Create the traitement variable based on the information entered
#' (stations and dates)
#'
#'  @param data dataframe
#'  @param station character
#'  @param dates Dates
#'  @param traitement character
#'
#' @return The return value is a character corresponding to the qualitative
#' variable of BACI.
#'
#'

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
