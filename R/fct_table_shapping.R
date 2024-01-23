#' table_shapping
#'
#' @description Creer les tables réduites à partir de la table complète
#' create data tables with only one explaineed variable with the explanatory
#' variables
#'
#' @return The return value is a list of 5 dataframe
#'
#'

table_shapping <- function(data) {
  Rich <- data %>% select(Richness, Annee=year, saison, traitement, campagne, station)
  Simp <- data %>% select(Simpson,  Annee=year, saison, traitement, campagne, station)
  Biom <- data %>% select(Biom, Annee=year, saison, traitement, campagne, station)
  Abun <- data %>% select(Abun, Annee=year, saison, traitement, campagne, station)
  list(data, Abun, Biom, Rich, Simp)
  return(list(data, Abun, Biom, Rich, Simp))
}
