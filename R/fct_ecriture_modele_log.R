#' ecriture_modele_log
#'
#' @description Fonction qui permet l'ecriture du modèle (formulation + loi) avec log transformation
#' 1. Sous forme output = language_model
#' 2. sous forme de formule pour faire tourner les fonctions glmms = formule_model
#' 3. les versions bis sont sans interactions
#' 4. la version formule_interaction est utilisé seulement pour le test de puissance
#' impossible de faire marcher la foncion quand on test l'intercation sinon
#'
#'
#' #' #' @return The return value, if any, from executing the function.
#'
#' @noRd

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
