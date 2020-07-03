require(gtools)

definePatterns<-function(dimension){
  d = c(1:dimension)
  symbol = matrix(unlist(permutations(n=dimension,r=dimension,v=d)),nrow = factorial(dimension),ncol = dimension,byrow = FALSE)
  symbol = symbol - 1
  symbol
}

formationPattern<-function(serie,dimension,delay,option){
  n_symbols = i = 1
  n = length(serie)
  p_patterns = elements = index2 = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  while(i <= n){    
    first = i
    if((i+dimension-1)<=n){
      index2[n_symbols,] = i:(i+dimension-1)
      elements[n_symbols,] = serie[i:(i+dimension-1)]
      p_patterns[n_symbols,] = index[order(elements[n_symbols,])]
      i = first + delay
      n_symbols = n_symbols + 1
    }else break
  }
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    return(p_patterns[1:dim(p_patterns)[1],])
  }else if(option == 1){
    elements = na.omit(elements)
    return(elements[1:dim(elements)[1],])    
  }else{
    index2 = na.omit(index2)
    return(index2[1:dim(index2)[1],])    
  }
}


distribution<-function(serie,dimension,delay){  
  fat <- factorial(dimension)
  probability <- rep(0,fat)
  p_patterns <- formationPattern(serie,dimension,delay,0)
  n_symbols <- dim(p_patterns)[1]
  symbols <- definePatterns(dimension)
  for(i in 1:fat){
    for(j in 1:n_symbols){
      if(all(p_patterns[j,] == symbols[i,])){ 
        probability[i] <- probability[i] + 1
        break
      }
    }
  }
  return(probability/n_symbols)
}





