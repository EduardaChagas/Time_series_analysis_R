library(combinat)
library(gtools)

#Calcula todos os possíveis vetores permutados, dado o ínicio e o final da subconjunto que deve ser permutado
mypermute<-function(vector, init, end){
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

dataDriven<-function(series, dimension, delay, mypatterns, elements, symbols, prob){
  patterns <- symbols
  fat <- factorial(dimension)
  weigth = rep(0,dim(symbols)[1])
  
  for(i in 1:dim(elements)[1]){
    
    aux = duplicated(elements[i,order(elements[i,])])
    isDuplicated = length(aux[aux==TRUE])
    
    if(isDuplicated != 0){ #Step 7: If the vector has equal components, break the ties add small pertubations 
      init = which(aux==TRUE)[1] - 1
      end = init + isDuplicated
      myresult = mypermute(mypatterns[i,],init,end)
      index = rep(0,dim(myresult)[1])
      
      for(j in 1:dim(myresult)[1]){
        for(k in 1:fat){
          if(all(myresult[j,] == symbols[k,])){ 
            index[j] = k 
            break
          }
        }
      }
      
      sumProb = sum(prob[index])
      if(sumProb != 0){
        weigth[index] = weigth[index] + (prob[index]/sumProb)
      }
    }else{
      for(k in 1:fat){
        if(all(mypatterns[i,] == symbols[k,])){ 
          weigth[k] = weigth[k] + 1
          break
        }
      }
    }
  }
  p = weigth/dim(elements)[1]
  if(sum(p)!=1){
    r = (1 - sum(p))/factorial(dimension)
    p = p + r
  }
  p
}