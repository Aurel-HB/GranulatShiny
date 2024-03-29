% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_ecriture_modele_log.R
\name{ecriture_modele_log}
\alias{ecriture_modele_log}
\title{ecriture_modele_log}
\usage{
ecriture_modele_log(y, interaction, methode, covariable, loi)
}
\arguments{
\item{y}{character = name of the explained variable}

\item{interaction}{boolean}

\item{methode}{character in (glm, glmm, permanova)}

\item{covariable}{character that list all the covariable in the model}

\item{loi}{character = name of the chosen probability distribution

 @return The return value is a list of all the information needed to run
 a model in log transformation}
}
\description{
Function for writing the model (formulation + distribution)
with log transformation
1. in output form = language_model
2. in formula form to run glmms functions = formula_model
3. the bis versions have no interactions
4. the formula_interaction version is only used for the power test
impossible to run the function when testing the interaction otherwise
}
