#' numeric_summary
#'
#' @description  summarise a numeric data
#'
#' @param vector a numeric vector
#' @param name a character value of the name of the variable in vector
#'
#' @return a summarised vector of the numeric vector composed of "variable",
#'  "n_missing", "n_total", "complete_rate", "mean","sd", "min", "Q25",
#'  "median", "Q75", "max"
#'  be careful the mean and the sd are round at 2 digits
#'
#' @export
#'

numeric_summary <- function(vector, name){
  var_summary <- as.numeric(summary(vector))
  mean <- round(mean(vector), digits = 2)
  sd <-  round(sd(vector), digits = 2)
  n0 <- 0
  for (value in vector){
    if (value == 0){
      n0 <- n0 + 1
    }
  }
  rate <- 1 - n0/length(vector)
  final_vector <- data.frame(name, n0,length(vector), rate, mean, sd, var_summary[1], var_summary[2],
                             var_summary[3], var_summary[5], var_summary[6])
  names(final_vector) <- c("variable", "n_missing", "n_total", "complete_rate",
                           "mean","sd", "min", "Q25","median", "Q75", "max")
  return(final_vector)
}
