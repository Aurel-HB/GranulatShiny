#' permanova_maker
#'
#' @description Fonction pour faire tourner les permanova en fonction de la formule creer par la fct ecriture_modele
#' L'intéraction est retirée automatiquement si non significative
#'
#'@param data description
#'@param formule description
#'@param formule_bis description
#'@param interaction description
#'
#' @return The return value is a list with the result of the permanova
#' with and without interaction
#'
#'

permanova_maker <-
  function(data,
           formule,
           formule_bis,
           interaction
  ) {
    mod <- c()
    if (interaction == F) {
      mod <-
        adonis(formule,
               data = data,
        )
      if (mod$aov.tab["traitement:saison",6] > 0.05) {
        mod_f <-
          adonis(formule_bis,
                 data = data,
          )
        list(mod, mod_f)
      } else {
        list(mod, mod)
      }
    }
    else if (interaction == T) {
      mod_f <-
        adonis(formule_bis,
               data = data,
        )
      list(mod, mod_f)
    }

  }
