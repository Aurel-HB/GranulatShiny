#' traitement_maker_zones
#'
#' @description Repete la fonction traitement_maker pour chaque sous-zone/periode
#' Repeat the traitement_maker function for each zone
#' Currently we work with 1 zone but with the future dev there might be others
#'
#' @param data dataframe
#' @param liste_station character
#' @param liste_dates dataframe
#' @param zones integer
#' @param traitement character
#'
#' @return No return value
#'
#'

traitement_maker_zones<-function(data, liste_station, liste_dates, zones, traitement){
  for (y in 1:as.numeric(zones)){
    traitement<-traitement_maker(data, liste_station[[y]], liste_dates[[y]], traitement)
  }
  as.factor(traitement)
}
