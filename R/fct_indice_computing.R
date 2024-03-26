#' indice_computing
#'
#' @description Calcul des indicateurs
#' calculate of the abundance, biomass, richness, shannon, simpson and
#' pielou indicators
#'
#' @param data_abun dataframe
#' @param data_biom dataframe
#'
#' @return a dataframe with the indicators
#'
#'

indice_computing <- function(data_abun, data_biom) {
  data_abun[is.na(data_abun)] <- 0
  data_biom[is.na(data_biom)] <- 0
  data_abun$Abun <-
    round(rowSums(data_abun[, -c(1:9)], na.rm = T), digits = 0) # Somme des abondances par ligne = abondance totale par trait
  data_abun <- data_abun %>% relocate(1:9, Abun)
  data_abun$Biom <-
    round(rowSums(data_biom[, -c(1:9)], na.rm = T), digits = 3) # Somme des biomasses par ligne = biomasse totale par trait
  data_abun <- data_abun %>% relocate(1:10, Biom)
  data_abun$Richness <-
    rowSums(data_abun[, -c(1:11)] != 0) # Calcul de la richesse par trait
  data_abun <- data_abun %>% relocate(1:11, Richness)
  data_abun$Shannon <-
    round(diversity(data_abun[, -c(1:12)], index = "shannon"), digits = 2)
  data_abun <- data_abun %>% relocate(1:12, Shannon)
  data_abun$Simpson <-
    round(diversity(data_abun[, -c(1:13)], index = "simpson"), digits = 2)
  data_abun <- data_abun %>% relocate(1:13, Simpson)
  data_abun$Pielou <-
    round(data_abun$Shannon / log(data_abun$Richness), digits = 2)
  data_abun <- data_abun %>% relocate(1:14, Pielou)

  #test na in pielou
  for(i in 1:length(data_abun$Pielou)){
    if(is.na(data_abun$Pielou[i])){
      data_abun$Pielou[i] <- 0
    }
  }

  return(data_abun)

}
