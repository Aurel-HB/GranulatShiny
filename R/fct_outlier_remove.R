#' outlier_remove
#'
#' @description Remove survey lines with extreme values of the calculated
#' indicators
#' the method is the Interquartile Range (IQR) (Walfish 2006, Yang et al. 2019)
#'
#' @param data dataframe
#'
#' @return The return value is a dataframe with the extrem value remove by the
#'  boxplot method
#'
#'

outlier_remove <- function(data) {
  a <- boxplot.stats(data$Richness)$out
  b <- boxplot.stats(data$Biom)$out
  c <- boxplot.stats(data$Abun)$out
  d <- boxplot.stats(data$Shannon)$out
  e <- boxplot.stats(data$Simpson)$out
  f <- boxplot.stats(data$Pielou)$out
  data_outlier_remove <-
    data[-c(
      data$Richness %in% a | data$Biom %in% b | data$Abun %in% c
      |
        data$Shannon %in% d | data$Simpson %in% e | data$Pielou %in% f
    ), ]
  return(data_outlier_remove)

}
