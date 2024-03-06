#' delete_outliers
#'
#' @description A fct function that remove the outliers of a vector using
#' the 1.5xIQR rule
#'
#' @param vector a numeric or integer vector
#'
#' @return The intitial vector without the outliers of the series
#'
#'

delete_outliers <- function(vector){
  # Calculate quartiles and IQR
  Q1 <- quantile(vector, 0.25)
  Q3 <- quantile(vector, 0.75)
  IQR_value <- IQR(vector)

  # Set the lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Filter the vector based on the bounds
  filtered_vector <- vector[vector >= lower_bound & vector <= upper_bound]

  # Print the result
  return(filtered_vector)
}
