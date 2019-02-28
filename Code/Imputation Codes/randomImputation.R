require(gtools)
require(DataDriven)
require(combinat)

randomImputation <- function(series, dimension, delay, patterns, elements, symbols){
  
  n_symbols <- dim(patterns)[1]
  weigth <- rep(0,dim(symbols)[1]) 
  fat <- factorial(dimension)
  
  for(i in 1:dim(elements)[1]){
    aux <- duplicated(elements[i,order(elements[i,])])
    isDuplicated <- length(aux[aux==TRUE])
    
    if(isDuplicated != 0){ 
      init <- which(aux==TRUE)[1] - 1 
      end <- init + isDuplicated
      myresult <- mypermute(patterns[i,],init,end)
      
      for(j in 1:dim(myresult)[1]){
        for(k in 1:fat){
          if(all(myresult[j,] == symbols[k,])){ 
            weigth[k] <- weigth[k] + (1/dim(myresult)[1])
            break
          }
        }
      }
    }else{
      for(k in 1:fat){
        if(all(patterns[i,] == symbols[k,])){ 
          weigth[k] = weigth[k] + 1
          break
        }
      }
    }
  }
  return(weigth/n_symbols)
  
}


