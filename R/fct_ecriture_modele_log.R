#' ecriture_modele_log
#'
#' @description Function for writing the model (formulation + distribution)
#' with log transformation
#' 1. in output form = language_model
#' 2. in formula form to run glmms functions = formula_model
#' 3. the bis versions have no interactions
#' 4. the formula_interaction version is only used for the power test
#' impossible to run the function when testing the interaction otherwise
#'
#' @param y character = name of the explained variable
#' @param interaction boolean
#' @param methode character in (glm, glmm, permanova)
#' @param covariable character that list all the covariable in the model
#' @param loi character = name of the chosen probability distribution
#'
#'  @return The return value is a list of all the information needed to run
#'  a model in log transformation
#'
#'

ecriture_modele_log <-
  function(y, interaction, methode, covariable, loi) {
    partie_1 <- "saison"
    if (!is.null(covariable)) {
      for (i in 1:length(covariable)) {
        partie_1 <- paste(partie_1, "+", covariable[i])
      }
    }

    if (methode=="1") {
      partie_2 <- "+ (1|campagne) + (1|station)"
    } else {
      partie_2 <- NULL
    }

    symbole <- ifelse(interaction == F, "*", "+")
    modele_ecriture <-
      paste("<b>", y, "~", "traitement", symbole, partie_1, partie_2, "</b>")
    if (methode=="3") {
      loi_modele<-
        paste("Avec", "<b>", y, "</b>", "ne suivant pas de loi définie :","<b>", "méthode non paramétrique", "</b>")
    } else {
      loi_modele <-
        paste("Avec", "<b>", y, "</b>", "suivant une loi", "<b>", loi,  "</b>")
    }
    formule_modele <-
      as.formula(paste("log(",y, ")~", "traitement *", partie_1, partie_2))
    language_modele <-
      parse(text=paste("log(",y, ")~", "traitement *", partie_1, partie_2))[[1]]
    formule_modele_bis <-
      as.formula(paste("log(",y, ")~", "traitement +", partie_1, partie_2))
    language_modele_bis <-
      parse(text=paste("log(",y, ")~", "traitement +", partie_1, partie_2))[[1]]
    # formule avec une variable interaction (necessaire pour test de puissance)
    formule_interaction<-as.formula(paste("log(",y,")~interaction + traitement +", partie_1, partie_2))
    language_interaction <-
      parse(text=paste("log(",y, ")~interaction + traitement +", partie_1, partie_2))[[1]]

    list(modele_ecriture,
         loi_modele,
         formule_modele,
         formule_modele_bis,
         language_modele,
         language_modele_bis,
         formule_interaction,
         language_interaction)

  }
