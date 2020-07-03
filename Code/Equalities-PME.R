equalities.values <- function(series){
  aux = duplicated(series)
  answer = length(aux[aux==TRUE])
  answer = (answer*100)/length(series)
  answer = format(round(answer, 3), nsmall = 3)
  return(paste(answer,"%"))
} 

removeDuplicate <- function(serie){
  serie = unique(serie)
  serie
}

min.entropy <- function(prob){
  pme = (-1)*log(max(prob))
  return(pme)
}

#falta adicionar no relatório
PMEBidimensional<-function(image,dimX,dimY,delX,delY){
  probability <- distributionImage(image,dimX,dimY,delX,delY)
  pme = (-1)*log(max(probability))
  pme
}