

#'rdist n fois
#'@export
#' @param x numeric vector representing the values of the random variable
#' @param p numeric vector representing the probabilities
#' @param n number of iterations

rdistn <-function(x,p,n)
{
  v <- c(1:n)
  for (j in 1:n) {
    y=rdist(x,p)
    v[j] <- y
    
  }
  return(v)
}

