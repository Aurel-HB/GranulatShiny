#' verification
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
