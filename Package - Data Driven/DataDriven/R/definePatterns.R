#' @title Calculates all ordinal patterns with a given dimension
#' @name definePatterns
#'
#' @description Returns the patterns with dimension d
#'
#' @param d Dimension size of ordinal patterns
#'
#' @return A numerical matrix with the patterns
#'
#' @author Eduarda Chagas
#'
#' d <- 3
#' definePatterns(d = d)
#' 
#'@import combinat
#'@import gtools
#'
definePatterns<-function(d){
  dd = c(1:d)
  symbol = matrix(unlist(permutations(n=d,r=d,v=dd)),nrow = factorial(d),ncol = d,byrow = FALSE)
  symbol = symbol - 1
  symbol
}