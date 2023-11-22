#' glmm_maker
#'
#' @description Fonction pour faire tourner les glmm en fonction de la formule creer par la fct ecriture_modele
#' L'intéraction est retirée automatiquement si non significative
#'
#' optimisation pour meilleur convergence : autoriasation des approximations ds dérivés partiels et nAGQ = 0
#' à modifier selon le besoin et l'évolution de la méthodologie
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
      if (interaction == F) {
        mod <-
          lmer(formule,
               data = data)
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            lmer(formule_bis,
                 data = data)
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          lmer(formule_bis,
               data = data)
        list(mod_f, mod_f)
      }
    } else {
      nAGQ <- 1L
      if (family == "Gamma log") {
        family <- Gamma(link = log)
        nAGQ <- 0
      } else if (family == "Gamma inverse") {
        family <- Gamma(link = "inverse")
        nAGQ <- 0
      }
      if (interaction == F) {
        mod <-
          glmer(formule,
                data = data,
                family = family,
                nAGQ = nAGQ)
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glmer(
              formule_bis,
              data = data,
              family = family,
              nAGQ = nAGQ
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
                family = family,
                nAGQ = nAGQ)
        list(mod_f, mod_f)
      }

    }
  }
