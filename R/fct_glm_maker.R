#' glm_maker
#'
#' @description Function to run the glm according to the formula created
#' by the fct ecriture_modele
#' Interaction is automatically removed if not significant
#'
#' In the case of glm, you need to modify the formula and the distribution
#' directly in the glm output object so that it is displayed correctly
#' in the model's output.
#'
#' @return The return value is a list of 2 result from the glmm initial or
#' final model (with or without interaction for example)
#'
#'

glm_maker <-
  function(data,
           formule,
           formule_bis, language, language_bis,
           interaction,
           family) {
    mod <- c()
    if (as.character(family) == "Binomiale négative") {
      if (interaction == F) {
        mod <-
          MASS::glm.nb(formule,
                       data = data)
        mod[["call"]][["formula"]]<-language
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            MASS::glm.nb(formule_bis,
                         data = data)
          mod_f[["call"]][["formula"]]<-language_bis
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }

      else if (interaction == T) {
        mod_f <-
          MASS::glm.nb(formule_bis,
                       data = data)
        mod_f[["call"]][["formula"]]<-language_bis
        list(mod_f, mod_f)
      }

    } else if (family == "Normale") {
      family <- gaussian(link = identity)
      family_bis<-parse(text="gaussian(link = identity)")[[1]]

      if (interaction == F) {
        mod <-
          glm(formule,
              data = data,
              family = family)
        mod[["call"]][["formula"]]<-language
        mod[["call"]][["family"]]<-family_bis
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glm(formule_bis,
                data = data,
                family = family)
          mod_f[["call"]][["formula"]]<-language_bis
          mod_f[["call"]][["family"]]<-family_bis
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glm(formule_bis,
              data = data,
              family = family)
        mod_f[["call"]][["formula"]]<-language_bis
        mod_f[["call"]][["family"]]<-family_bis
        list(mod_f, mod_f)
      }
      #if (interaction == F) {
      #  mod <-
      #    lm(formule,
      #       data = data)
      #  mod[["call"]][["formula"]]<-language
      #  if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
      #    mod_f <-
      #      lm(formule_bis,
      #         data = data)
      #    mod_f[["call"]][["formula"]]<-language_bis
      #    list(mod, mod_f)
      #  } else {
      #    list(mod, mod)
      #  }
      #}
      #else if (interaction == T) {
      #  mod_f <-
      #    lm(formule_bis,
      #       data = data)
      #  mod_f[["call"]][["formula"]]<-language_bis
      #  list(mod_f, mod_f)
      #}
    } else if (family == "Lognormale") {
        family <- gaussian(link = identity)
        family_bis<-parse(text="gaussian(link = identity)")[[1]]

      if (interaction == F) {
        mod <-
          glm(formule,
              data = data,
              family = family)
        mod[["call"]][["formula"]]<-language
        mod[["call"]][["family"]]<-family_bis
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glm(formule_bis,
                data = data,
                family = family)
          mod_f[["call"]][["formula"]]<-language_bis
          mod_f[["call"]][["family"]]<-family_bis
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glm(formule_bis,
              data = data,
              family = family)
        mod_f[["call"]][["formula"]]<-language_bis
        mod_f[["call"]][["family"]]<-family_bis
        list(mod_f, mod_f)
      }
    } else {
      if (family == "Gamma log") {
        family <- Gamma(link = log)
        family_bis<-parse(text="Gamma(link = log)")[[1]]

      } else if (family == "Gamma inverse") {
        family <- Gamma(link = "inverse")
        family_bis<-parse(text="Gamma(link = inverse)")[[1]]
      } else {
        family_bis<-parse(text=family)[[1]]
      }
      if (interaction == F) {
        mod <-
          glm(formule,
              data = data,
              family = family)
        mod[["call"]][["formula"]]<-language
        mod[["call"]][["family"]]<-family_bis
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            glm(formule_bis,
                data = data,
                family = family)
          mod_f[["call"]][["formula"]]<-language_bis
          mod_f[["call"]][["family"]]<-family_bis
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          glm(formule_bis,
              data = data,
              family = family)
        mod_f[["call"]][["formula"]]<-language_bis
        mod_f[["call"]][["family"]]<-family_bis
        list(mod_f, mod_f)
      }

    }
  }
