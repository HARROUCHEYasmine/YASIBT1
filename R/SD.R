
#'statistiques descriptives
#'@export
#' @param x numeric vector representing the values of the random variable



SD<-function(x)
{
  par(mfrow= c(1,3))
  hist(x, col = rainbow(10))
  boxplot(x, col = 'blue' )
  par(mfrow = c(2, 2))
  data.frame( min = min(x),
              max = max(x),
              median = median(x),
              mean = mean(x))
}