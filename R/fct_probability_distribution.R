#' probability_distribution
#'
#' @description A fct function that take in input the type of distribution
#' and calculate the values of the probability distribution with the parameters
#' of a database
#'
#' @param vector an integer
#' @param law an character with the name of the probability distribution
#'
#' @return a vector density of a distribution probability
#'
#'

probability_distribution <- function(vector, law){

  moy <- mean(vector)
  sd <- sd(vector)
  nb <- length(vector)

  if (law == "Normale"){
    dist_vector <- dnorm(vector, mean = moy, sd= sd)
  }
  if (law == "Binomiale"){
    dist_vector <- dbinom(vector, size = nb, prob = moy)
  }
  if (law == "Poisson"){
    dist_vector <- dpois(vector,lambda = moy)
  }
  if (law == "Binomiale négative"){
    dist_vector <- dnbinom(vector, mu = moy, size =  1/((sd**2-moy)/moy**2))
  }
  if (law == "Gamma log"){
    dist_vector <- dgamma(vector, shape = (moy**2/sd**2), scale = moy/sd**2)
  }
  if (law == "Lognormale"){
    dist_vector <- dlnorm(vector, meanlog = log(moy/sqrt(sd**2/moy**2 + 1)),
                          sdlog = log(sd**2/moy**2 + 1))
  }

  return(dist_vector)

}
