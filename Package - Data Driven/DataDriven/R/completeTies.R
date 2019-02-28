#' @title Implements complete case methodology 
#' @name completeTies
#'
#' @description Calculates the probability distribution using the complete case methodology, that is, using only the patterns formed without repeated elements.
#'
#' @param elements Sub-sets of elements of the time series with dimension d of delay t.
#' @param pattern Ordinal patterns formed from the time series with dimension d of delay t.
#'
#' @return The distribution of probability without considering the patterns formed of repeated elements
#'
#' @author Eduarda Chagas
#'
#' set.seed(1234567890, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' elem <- formationPattern(x = x, dimension = d, delay = del, option = 0)
#' pat <- formationPattern(x = x, dimension = d, delay = del, option = 1)
#' completeTies(elements, pattern)
#'
completeTies <- function(elements,pattern){
  symbols <- definePatterns(dim(elements)[2])
  fat = factorial(dim(elements)[2])
  p1 = rep(0,fat)
  re = 0
  for(i in 1:dim(elements)[1]){
    aux = duplicated(elements[i,])
    isDuplicated = length(aux[aux==TRUE])
    if(isDuplicated==0){
      re = re + 1
      for(j in 1:fat){
        if(all(pattern[i,] == symbols[j,])){
          p1[j]=p1[j]+1 
        }
      }
    }
  }
  return(p1/re)
}
