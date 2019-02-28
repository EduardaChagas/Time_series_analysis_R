#' @title Calculates all possible combinations of a set by exchanging only a subset of elements
#' @name myPermute
#'
#' @description Returns a numeric array containing all possible permutations of a vector by varying only a given subset.
#'
#' @param vector A numeric vector
#' @param init Beginning of subset
#' @param end End of subset
#'
#' @return Numerical matrix with permutations performed.
#'
#' @author Eduarda Chagas
#'
#' x <- c(1:10)
#' i <- 2
#' e <- 6
#' mypermute(vector = x, init = i, end = e)
#'
myPermute<-function(vector, init, end){
  a = matrix(unlist(permn(vector[init:end])),nrow = factorial(end-init+1),ncol = end-init+1,byrow = TRUE)
  number = dim(a)[1]
  result = matrix(nrow = number,ncol = length(vector))
  for(i in 1:number){
    if(1 < init){  
      result[i,1:(init-1)] = vector[1:(init-1)]
    }
    result[i,init:end] = a[i,]
    if(end < length(vector)){
      result[i,(end+1):length(vector)] = vector[(end+1):length(vector)]
    }
  }
  result
}