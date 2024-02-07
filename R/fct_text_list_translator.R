#' text_list_translator
#'
#' @description This function compiles all the long text present in the app
#' It create a table with a language per column
#' To add message, you write text into a character vector for each language
#' and then you assemble the vectors into a table
#'
#' @return The return value is a table that you can export in a repertory
#' and charge top choose the message you want to display
#'
#' @export

text_list_translator <- function(){

  fr <- c(
    "L'outil antérieur construit par Mathis Cambreling fonctionne seulement pour le jeu de données ayant servi de base à ses calculs. L'outil n'étant pas généralisable, celui-ci a été retiré pour assurer la stabilité actuelle de l'application. Un autre outil est en cours de développement."
  )

  en <- c(
    "The previous tool built by Mathis Cambreling only works for the dataset used as the basis for his calculations. As the tool cannot be generalised, it has been withdrawn to ensure the current stability of the application. Another tool is currently under development."
    )

  list_translate <- data.frame(fr,en)

  saveRDS(list_translate, "data/liste_translate.rds")
}
