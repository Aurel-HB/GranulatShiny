#' global_function
#'
#' @description Regroupe toutes les fonctions pour la mise en forme et le calcul des indicateurs
#' group all the functions used to format the data and calculate the indicators
#'
#' @param tutti_catch dataframe
#' @param tutti_operation dataframe
#' @param liste_station character
#' @param liste_dates dataframe
#' @param zones integer
#'
#' @return a list of 5 datatable
#'
#'

global_function <- function(tutti_catch, tutti_operation, liste_station, liste_dates, zones) {
  data <- tutti_function_traitement(tutti_catch, tutti_operation, liste_station, liste_dates, zones)
  dataf <- indice_computing(data[[1]], data[[2]]) #garde la table Abun
  dataff <- outlier_remove(dataf)
  data_list <- table_shapping(dataff)
  return(data_list)
}
