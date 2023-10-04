
#' puissance_maker 
#'
#' @description Simule de nouvelles campagnes et creer une courbe de puissance à partir des modèles spécifiques
#' le nombre de simulation ou le test pour estimer la pvalue sont à modifier ici
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# 
# 

puissance_maker <- function(model, fixeff, n, campagne, methode, data) {
  df <- data
  if (methode == "1") {
    model@call[["data"]] <- df
  } else {
    model[["call"]][["data"]] <- df
  }
  mod_extend <- simr::extend(model, along = "campagne", n = n + campagne)
  power_curve <-
    simr::powerCurve(
      mod_extend,
      along = "campagne",
      test = simr::fixed(fixeff, c("chisq", "lr")[as.numeric(methode)]),
      nsim = 200
    )
  
}


