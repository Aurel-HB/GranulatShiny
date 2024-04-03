#' glmm_maker
#'
#' @description Function to run the glmm according to the formula created
#' by the fct ecriture_modele
#' Interaction is automatically removed if not significant
#'
#' optimisation for better convergence: authorisation of partial derivative
#' approximations and nAGQ = 0
#' to be modified as required and as the methodology evolves
#'
#' @return The return value is a list of 2 result from the glmm initial or
#' final model (with or without interaction for example)
#'

glmm_maker <-
  function(data,
           formule,
           formule_bis,
           interaction,
           family) {
    mod <- c()
    if (as.character(family) == "Binomiale négative") {
      if (interaction == F) {
        mod <-
          glmer.nb(
            formule,
            data = data,
            nb.control = glmerControl(calc.derivs = F),
            nAGQ = 0
          )

        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glmer.nb(
              formule_bis,
              data = data,
              nb.control = glmerControl(calc.derivs = F),
              nAGQ = 0
            )
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }

      else if (interaction == T) {
        mod_f <-
          glmer.nb(
            formule_bis,
            data = data,
            nb.control = glmerControl(calc.derivs = F),
            nAGQ = 0
          )
        list(mod_f, mod_f)
      }

    } else if (family == "Normale") {
      family <- gaussian(link = identity)
      if (interaction == F) {
        mod <-
          glmer(formule,
               data = data,
               family = family)
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glmer(formule_bis,
                 data = data,
                 family = family)
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glmer(formule_bis,
               data = data,
               family = family)
        list(mod_f, mod_f)
      }
    } else if (family == "Lognormale") {
      family <- gaussian(link = identity)
      if (interaction == F) {
        mod <-
          glmer(formule,
               data = data,
               family = family)
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glmer(formule_bis,
                 data = data,
                 family = family)
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glmer(formule_bis,
               data = data,
               family = family)
        list(mod_f, mod_f)
      }
    } else {
      nAGQ <- 1L
      if (family == "Gamma log") {
        family <- Gamma(link = log)
      } else if (family == "Gamma inverse") {
        family <- Gamma(link = "inverse")
      }
      if (interaction == F) {
        mod <-
          glmer(formule,
                data = data,
                family = family)
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glmer(
              formule_bis,
              data = data,
              family = family
            )
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glmer(formule_bis,
                data = data,
                family = family)
        list(mod_f, mod_f)
      }

    }
  }
