if(!require("DataDriven")) install.packages("DataDriven_0.0.0.9000.tar.gz", repos = NULL, type="source")
if(!require("gtools")) install.packages("gtools")


complete.case <- function(series, dimension, delay){
  fat = factorial(dimension)
  probability = rep(0,fat)
  myPatterns = sum = 0
  
  symbols = definePatterns(dimension)
  patterns = formationPattern(series, dimension, delay, 0)
  elements = formationPattern(series, dimension, delay, 1)
  
  for(i in 1:dim(patterns)[1]){
    aux = duplicated(elements[i,order(elements[i,])])
    isDuplicated = length(aux[aux==TRUE])
    if(isDuplicated == 0){ 
      sum = sum + 1
      for(j in 1:fat){
        if(all(patterns[i,] == symbols[j,])){
          probability[j] = probability[j] + 1
          break
        }
      }
    }
  }
  return(probability/sum)
}