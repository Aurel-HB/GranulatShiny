% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_diversity_table.R
\name{diversity_table}
\alias{diversity_table}
\title{diversity_table}
\usage{
diversity_table(data, var_name)
}
\arguments{
\item{data}{dataframe}

\item{var_name}{character of 1 value}
}
\value{
The return value is a dataframe with the average value of chosen variable
for interior, exterior of the concession and the globale
}
\description{
A fct function that create a dataframe of 3 columns.
For a choosen variable we have the mean per survey per location.
The aim is to create a dataframe compose of 5 indictors
with 3 modalities each : (Abun,Biom,Richness,Shannon,Simpson) ; (int,ext,tot)
}
