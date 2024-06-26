% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_numeric_summary.R
\name{numeric_summary}
\alias{numeric_summary}
\title{numeric_summary}
\usage{
numeric_summary(vector, name)
}
\arguments{
\item{vector}{a numeric vector}

\item{name}{a character value of the name of the variable in vector}
}
\value{
a summarised vector of the numeric vector composed of "variable",
 "n_missing", "n_total", "complete_rate", "mean","sd", "min", "Q25",
 "median", "Q75", "max"
 be careful the mean and the sd are round at 2 digits
}
\description{
summarise a numeric data
}
