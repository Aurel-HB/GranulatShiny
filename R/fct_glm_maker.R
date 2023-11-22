#' glm_maker
#'
#' @description Fonction pour faire tourner les glm en fonction de la formule creer par la fct ecriture_modele
#' L'intéraction est retirée automatiquement si non significative
#'
#' Dans le cas des glm besoin de modifier la formule et la loi directement dans l'objet de sortie des glm pour que ca s'affiche correctement dans les sorties du modele
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
      if (interaction == F) {
        mod <-
          lm(formule,
             data = data)
        mod[["call"]][["formula"]]<-language
        if (Anova(mod, type = "III")["traitement:saison", 3] > 0.05) {
          mod_f <-
            lm(formule_bis,
               data = data)
          mod_f[["call"]][["formula"]]<-language_bis
          list(mod, mod_f)
        } else {
          list(mod, mod)
        }
      }
      else if (interaction == T) {
        mod_f <-
          lm(formule_bis,
             data = data)
        mod_f[["call"]][["formula"]]<-language_bis
        list(mod_f, mod_f)
      }
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
