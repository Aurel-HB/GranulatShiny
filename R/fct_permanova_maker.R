#' permanova_maker
#'
#' @description Fonction pour faire tourner les permanova en fonction de la formule creer par la fct ecriture_modele
#' L'intéraction est retirée automatiquement si non significative
#'
#' @param data dataframe
#' @param formule expression from parse
#' @param formule_bis expression from parse
#' @param interaction Boolean
#' @param dist matrix or large list
#'
#' @return The return value is a list with the result of the permanova
#' with and without interaction
#'
#'

permanova_maker <-
  function(data,
           formule,
           formule_bis,
           interaction,
           dist
  ) {
    mod <- c()
    formule <- as.formula(formule)
    formule_bis <- as.formula(formule_bis)
    if (interaction == F) {
      mod <-
        adonis2(formule,
               data = data,
               permutations = 999
        )
      attr(mod, "heading")[2] <- paste("adonis2(formula = ",
                                       formule[2],formule[1],formule[3],
                                       " data = data, permutations = 999)",
                                       sep = "")
      if (mod["traitement:saison","Pr(>F)"] > 0.05) {
        mod_f <-
          adonis2(formule_bis,
                 data = data,
                 permutations = 999
          )
        attr(mod_f, "heading")[2] <- paste("adonis2(formula = ",
                                          formule_bis[2],formule_bis[1],formule_bis[3],
                                         " data = data, permutations = 999)",
                                         sep = "")
        list(mod, mod_f)
      } else {
        list(mod, mod)
      }
    }
    else if (interaction == T) {
      mod_f <-
        adonis2(formule_bis,
               data = data,
               permutations = 999
        )
      attr(mod_f, "heading")[2] <- paste("adonis2(formula = ",
                                        formule_bis[2],formule_bis[1],formule_bis[3],
                                        " data = data, permutations = 999)",
                                        sep = "")
      list(mod, mod_f)
    }

  }


#prepare for adonis
# vector <- decostand(species["Limanda.limanda"],"chi.square", MARGIN = 2)
# vector <- data.frame(as.numeric(vector[1,]))
# names(vector) <- "species"
# dist <- vegdist(vector, method = "euclidean")
# result <- adonis2(dist~traitement*saison, data = data.Table.complete, permutations = 999)



