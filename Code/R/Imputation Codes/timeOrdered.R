timeOrdered <- function(serie,dimension,delay, p_patterns, symbols){
  fat <- factorial(dimension)
  probability <- rep(0,fat)
  n_symbols <- dim(p_patterns)[1]
  a = 0
  for(j in 1:n_symbols){
    for(i in 1:fat){
      if(all(p_patterns[j,] == symbols[i,])){
        a = a + 1
        probability[i] <- probability[i] + 1
        break
      }
    }
  }
  return(probability/n_symbols)
}


