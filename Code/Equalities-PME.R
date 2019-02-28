equalitiesValues<-function(serie){
  aux = duplicated(serie)
  answer = length(aux[aux==TRUE])
  answer = (answer*100)/length(serie)
  answer = format(round(answer, 3), nsmall = 3)
  return(paste(answer,"%"))
} 

removeDuplicate <- function(serie){
  serie = unique(serie)
  serie
}

PME<-function(p){
  pme = (-1)*log(max(p))
  pme
}

#falta adicionar no relatório
PMEBidimensional<-function(image,dimX,dimY,delX,delY){
  probability <- distributionImage(image,dimX,dimY,delX,delY)
  pme = (-1)*log(max(probability))
  pme
}