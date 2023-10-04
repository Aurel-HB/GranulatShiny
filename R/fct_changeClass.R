#' changeClass 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

changeClass<-function (df, new) 
{
  assert_that(is.data.frame(df), dim(df)[2] == length(new), 
              is.character(new))
  if (!all(new %in% c("num", "char", "fac", "e"))) 
    stop("One or more of new classes is not defined appropriately as either 'num', 'char', 'fac' or 'e'.")
  for (i in 1:length(new)) {
    if (new[i] == "num") {
      if (is.factor(df[, i])) {
        df[, i] <- as.numeric(levels(df[, i]))[df[, 
                                                  i]]
      }
      else {
        df[, i] <- as.numeric(df[, i])
      }
    }
    else if (new[i] == "char") {
      df[, i] <- as.character(df[, i])
    }
    else if (new[i] == "fac") {
      df[, i] <- as.factor(df[, i])
    }
    else {
      df[, i] <- df[, i]
    }
  }
  return(df)
}
