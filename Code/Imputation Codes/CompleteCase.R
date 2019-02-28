require(gtools)
library(DataDriven)

completeCaseFunction <- function(series, dimension, delay, patterns, elements, symbols){
  fat <- factorial(dimension)
  probability <- rep(0,fat)
  myPatterns <- sum <- a <-0
  
  for(i in c(1:dim(patterns)[1])){
    aux <- duplicated(elements[i,order(elements[i,])])
    isDuplicated <- length(aux[aux==TRUE])
    if(isDuplicated == 0){ 
      sum = sum + 1
      #init = patterns[i,1]*(dim(symbols)[1]/dimension) + 1
      #end = init + (dim(symbols)[1]/dimension) - 1
      for(j in 1:fat){
        if(all(patterns[i,] == symbols[j,])){
          a = a + 1
          probability[j] <- probability[j] + 1
          break
        }
      }
    }
  }
  return(probability/sum)
}