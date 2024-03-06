#' summary_report
#'
#' @description A fct function that create a list to make  easier
#' the exportation of the summary inforamtion
#'
#' @param summary summary of a model
#'
#' @return The return value is a list of a ceofficient dataframe, a statistic
#' indicators dataframe and the formula as a character
#'
#'
# Function to create a summary report for a GLM
create_glm_summary_report <- function(summary) {
  # Extract formula
  formula_text <- summary$call

  # Extract coefficients
  coefficients_table <- as.data.frame(summary$coefficients)

  # Extract model fit statistics
  fit_statistics_table <- data.frame(
    Statistic = c("AIC", "Residual eviance", "Null deviance"),
    Value = c(summary$aic, summary$deviance, summary$null.deviance)
  )

  # Combine into a character
  summary_report <- list(formula_text, coefficients_table,
                         fit_statistics_table
  )

  return(summary_report)
}
