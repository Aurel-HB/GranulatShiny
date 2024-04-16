#' diversity_table
#'
#' @description A fct function that create a dataframe of 3 columns.
#' For a choosen variable we have the mean per survey per location.
#' The aim is to create a dataframe compose of 5 indictors
#' with 3 modalities each : (Abun,Biom,Richness,Shannon,Simpson) ; (int,ext,tot)
#'
#'
#' @param data dataframe
#' @param var_name character of 1 value
#'
#' @return The return value is a dataframe with the average value of chosen variable
#' for interior, exterior of the concession and the globale
#'
#' @export

diversity_table <- function(data, var_name){

  #list stations concession
  data_concession <- data %>% dplyr::filter(traitement == "Impact")
  liste_station <- unique(data_concession$station)


  # Abun interior and exterior the concession
  nb_campagne <- max(data$campagne)
  Var_int <- data.frame(rep(0,nb_campagne), rep(0,nb_campagne))
  Var_ext <-data.frame(rep(0,nb_campagne),rep(0,nb_campagne))
  Var_tot <-data.frame(rep(0,nb_campagne),rep(0,nb_campagne))

  names(Var_int) <- c(paste(var_name,"_int_mean", sep = ""),
                   paste(var_name,"_int_sd", sep=""))
  names(Var_ext) <- c(paste(var_name,"_ext_mean", sep=""),
                   paste(var_name,"_ext_sd", sep=""))
  names(Var_tot) <- c(paste(var_name,"_tot_mean", sep=""),
                   paste(var_name,"_tot_sd", sep=""))

  for (i in sort(unique(data$campagne))){
    temp <- data %>% dplyr::filter(campagne == i)
    int <- c()
    ext <- c()
    tot <- c()
    for (j in 1:nrow(temp)){
      if( temp$station[j] %in% liste_station){
        int <- as.numeric(c(int,temp[var_name][j,1]))
      }
      else{
        ext <- as.numeric(c(ext, temp[var_name][j,1]))
      }
      tot <- as.numeric(c(tot, temp[var_name][j,1]))
    }
    Var_int[i,1] <- round(mean(int), digits = 3)
    Var_int[i,2] <- round(stats::sd(int), digits = 3)
    Var_ext[i,1] <- round(mean(ext), digits = 3)
    Var_ext[i,2] <- round(stats::sd(ext), digits = 3)
    Var_tot[i,1] <- round(mean(tot), digits = 3)
    Var_tot[i,2] <- round(stats::sd(tot), digits = 3)
  }


  return(data.frame(Var_int,Var_ext,Var_tot))
}

