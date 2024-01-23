#' verification
#'
#' @description A fct function that check if the columns name of a dataset
#' correspond to attempted names
#' So it check if a table is in the good shape
#'
#' @param name character
#' @param format character
#'
#' @return The return value is a booleen
#'
#'

verification <- function(name, format){
  test <- c(name == format)
  booleen = TRUE
  for (var in test){
    if (var == FALSE){
      booleen = FALSE
    }
  }
  return(booleen)
}
