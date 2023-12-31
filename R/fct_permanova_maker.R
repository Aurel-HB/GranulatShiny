#' permanova_maker 
#'
#' @description Fonction pour faire tourner les permanova en fonction de la formule creer par la fct ecriture_modele
#' L'intéraction est retirée automatiquement si non significative
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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

