search <- function(pattern, symbols, dimension){
  fat <- factorial(dimension)
  init = pattern[1]*(fat/dimension) + 1
  end = init + (fat/dimension)
  for(i in c(init, end)){
    if(all(pattern == symbols[i,])){
      return(i)
    }
  }
}