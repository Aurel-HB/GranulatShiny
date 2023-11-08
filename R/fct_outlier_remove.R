#' outlier_remove
#'
#' @description Eneleve les traits qui possèdent des valeurx extremes des indicateurs calculés
#' reprise de la méthode de jehanne pour son article, peut etre à revoir ou enelever
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
