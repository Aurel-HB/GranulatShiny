#' diversity_table
#'
#' @description A fct function that create a dataframe compose of 5 indictors
#' with 3 modalities each : (Abun,Biom,Richness,Shannon,Simpson) ; (int,ext,tot)
#'
#' @param data dataframe
#' @param var_name character of 1 value
#'
#' @return The return value is a dataframe with the value of chosen variable
#' for interior, exterior of the concession and the globale average
#'
#'

diversity_table <- function(data, var_name){

  #list stations concession
  data_concession <- data %>% dplyr::filter(traitement == "Impact")
  liste_station <- unique(data_concession$station)


  # Abun interior and exterior the concession
  nb_campagne <- max(data$campagne)
  Var_int <- data.frame(rep(0,nb_campagne), rep(0,nb_campagne))
  Var_ext <-data.frame(rep(0,nb_campagne),rep(0,nb_campagne))
  Var_tot <-data.frame(rep(0,nb_campagne),rep(0,nb_campagne))

  names(Var_int) <- c(paste(var_name,"_int_value", sep = ""),
                   paste(var_name,"_int_sd", sep=""))
  names(Var_ext) <- c(paste(var_name,"_ext_value", sep=""),
                   paste(var_name,"_ext_sd", sep=""))
  names(Var_tot) <- c(paste(var_name,"_tot_value", sep=""),
                   paste(var_name,"_tot_sd", sep=""))

  for (i in sort(unique(data$campagne))){
    temp <- data %>% dplyr::filter(campagne == i)
    int <- c()
    ext <- c()
    tot <- c()
    for (j in 1:nrow(temp)){
      if( temp$station[j] %in% liste_station){
        int <- c(int,temp[var_name][j,1])
      }
      else{
        ext <- c(ext, temp[var_name][j,1])
      }
      tot <- c(tot, temp[var_name][j,1])
    }
    Var_int[i,1] <- mean(int)
    Var_int[i,2] <- stats::sd(int)
    Var_ext[i,1] <- mean(ext)
    Var_ext[i,2] <- stats::sd(ext)
    Var_tot[i,1] <- mean(tot)
    Var_tot[i,2] <- stats::sd(tot)
  }


  return(data.frame(Var_int,Var_ext,Var_tot))
}

