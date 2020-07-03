time.ordered <- function(series, dimension, delay){
  
  symbols = definePatterns(dimension)
  patterns = formationPattern(series, dimension, delay, 0)
  
  fat = factorial(dimension)
  probability = rep(0,fat)
  n.symbols = dim(patterns)[1]
  
  for(j in 1:n.symbols){
    for(i in 1:fat){
      if(all(patterns[j,] == symbols[i,])){
        probability[i] = probability[i] + 1
        break
      }
    }
  }
  
  return(probability/n.symbols)
}


