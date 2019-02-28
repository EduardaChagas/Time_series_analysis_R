#' @title Form the ordinal patterns of a time series
#' @name formationPattern
#'
#' @description Divides the time series into subsets with a certain size and delay and calculates its ordinal patterns.
#'
#' @param serie A numeric vector (e.g. a time series)
#' @param dimension Dimension size of ordinal patterns
#' @param delay Size of the delay of ordinal patterns
#' @param option Determines the data set to be returned. The parameter must be 0 for ordinal patterns formed or different from 0 to return the subsets of formed elements.
#'
#' @return A numerical vector containing ordinal patterns or subsets of formed elements.
#'
#' @author Eduarda Chagas
#'
#' set.seed(1234567890, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' opt <- 0
#' formationPattern(serie = x, dimension = d, delay = del, option = opt)
#' 
#'@import combinat
#'@import gtools
#'
formationPattern<-function(serie,dimension,delay,option){
  n_symbols = i = 1
  n = length(serie)
  p_patterns = elements = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  while(i <= n){    
    first = i
    if((i+dimension-1)<=n){
      elements[n_symbols,] = serie[i:(i+dimension-1)]
      p_patterns[n_symbols,] = index[order(elements[n_symbols,])]
      i = first + delay
      n_symbols = n_symbols + 1
    }else break
  }
  if(option == 0){
    p_patterns = stats::na.omit(p_patterns)
    p_patterns[1:dim(p_patterns)[1],]
  }else{
    elements = stats::na.omit(elements)
    elements[1:dim(elements)[1],]    
  }
}
