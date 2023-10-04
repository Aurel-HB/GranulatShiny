#' glm_maker_puissance 
#'
#' @description Modèle glm creer pour la puissance stat avec formule_interaction
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

glm_maker_puissance <-
  function(data,
           formule,
           formule_bis, language, language_bis,
           family) {
    mod <- c()
    data_complet<-data
    if (as.character(family) == "Binomiale négative") {
      mod <-
        MASS::glm.nb(formule,
                     data = data)
      mod[["call"]][["formula"]]<-language
      mod_f <-
        MASS::glm.nb(formule_bis,
                     data = data)
      mod_f[["call"]][["formula"]]<-language_bis 
      list(mod, mod_f)
    }  else if (family == "Normale") {
      
      mod <-
        lm(formule,
           data = data)
      mod[["call"]][["formula"]]<-language
      mod_f <-
        lm(formule_bis,
           data = data)
      mod_f[["call"]][["formula"]]<-language_bis 
      list(mod, mod_f)
      
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
      
      mod <-
        glm(formule,
            data = data,
            family = family)
      mod[["call"]][["formula"]]<-language
      mod[["call"]][["family"]]<-family_bis
      mod_f <-
        glm(formule_bis,
            data = data,
            family = family)
      mod_f[["call"]][["formula"]]<-language_bis 
      mod_f[["call"]][["family"]]<-family_bis
      list(mod, mod_f)
    } 
}
