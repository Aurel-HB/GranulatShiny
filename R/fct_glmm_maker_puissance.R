#' glmm_maker_puissance
#'
#' @description Modèle glmm creer pour la puissance stat avec formule_interaction
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

glmm_maker_puissance <-
  function(data,
           formule,
           formule_bis,
           family) {
    mod <- c()
    if (as.character(family) == "Binomiale négative") {
      mod <-
        glmer.nb(
          formule,
          data = data,
          nb.control = glmerControl(calc.derivs = F),
          nAGQ = 0
        )
      mod2 <-
        glmer.nb(
          formule_bis,
          data = data,
          nb.control = glmerControl(calc.derivs = F),
          nAGQ = 0
        )
      
      list(mod, mod2)
  } else if (family == "Normale") {
    mod <-
      lmer(formule,
           data = data)
    mod2 <-
      lmer(formule_bis,
           data = data)
    list(mod, mod2)
    
  } else {
    nAGQ <- 1L
    if (family == "Gamma log") {
      family <- Gamma(link = log)
      nAGQ <- 0
    } else if (family == "Gamma inverse") {
      family <- Gamma(link = "inverse")
      nAGQ <- 0
    }
    mod <-
      glmer(formule,
            data = data,
            family = family,
            nAGQ = nAGQ)
    mod2 <-
      glmer(formule_bis,
            data = data,
            family = family,
            nAGQ = nAGQ)
    list(mod, mod2)
    
  }
}
