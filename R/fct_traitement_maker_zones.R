#' traitement_maker_zones
#'
#' @description Repete la fonction traitement_maker pour chaque sous-zone/periode
#' A adapter selon la méthodologie définie
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

traitement_maker_zones<-function(data, liste_station, liste_dates, zones, traitement){
  for (y in 1:as.numeric(zones)){
    traitement<-traitement_maker(data, liste_station[[y]], liste_dates[[y]], traitement)
  }
  as.factor(traitement)
}
