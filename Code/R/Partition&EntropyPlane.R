library(ggplot2)
library(combinat)

entropyPlane<-function(serie,partition,dimension,delay,distribution,option,q=0){
  entropy = rep(0,partition)
  division = floor(length(serie)/partition)
  init = 1
  for(i in 1:partition){
    myPartition = serie[init:(init+division)]
    if(distribution == 1){
      probability = distribution(myPartition,dimension,delay)
    }else{
      probability = WPE(myPartition,dimension,delay)
    }
    if(option == 1){
      entropy[i] = shannonNormalized(probability)
    }else if(option == 2){
      entropy[i] = tsallisEntropy(probability,q)
    }else if(option == 3){
      entropy[i] = renyiEntropy(probability,q)
    }else{
      entropy[i] = PMEUnidimensional(probability)
    }
    init = init + division + 1
  }
  #png("myEntropy.png")
  if(partition==1){
    p = qplot(x=c(1:partition),y=entropy,geom="point",xlab="Partitions-Time Series",ylab="Entropy") +
      ggtitle("Permutation Entropy Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  else{
    p = qplot(x=c(1:partition),y=entropy,geom="line",xlab="Partitions-Time Series",ylab="Entropy") +
      ggtitle("Permutation Entropy Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  print(p)
  #dev.off()
  return(entropy)
}

#Falta adicionar no relatório
distancePlane<-function(serie,partition,dimension,delay,option=1,optionP=1,q=1){
  distance = rep(0,partition)
  division = floor(length(serie)/partition)
  init = 1
  for(i in 1:partition){
    myPartition = serie[init:(init+division)]
    if(optionP == 1){
      probability = distribution(myPartition,dimension,delay)
    }else{
      probability = WPE(myPartition,dimension,delay)
    }
    if(option == 1){
      distance[i] = euclidianDistance(probability)
    }else if(option == 2){
      distance[i] = squaredDistance(probability)
    }else if(option == 3){
      distance[i] = manhattanDistance(probability)
    }else if(option == 4){
      distance[i] = chebyshevDistance(probability)
    }else if(option == 5){
      distance[i] = kullbackDivergence(probability)
    }else if(option == 6){
      distance[i] = hellingerDistance(probability)
    }else if(option == 7){
      distance[i] = jensenDivergence(probability)
    }else if(option == 8){
      distance[i] = woottersDistance(probability,q)
    }else if(option == 9){
      distance[i] = bhattacharyyaDistance(probability,q)
    }else{
    cat("Distance option unavailable\n")
    }
    init = init + division + 1
  }
  #png("myEntropy.png")
  if(partition==1){
    p = qplot(x=c(1:partition),y=distance,geom="point",xlab="Partitions-Time Series",ylab="Distance") +
      ggtitle("Stochastic distance Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  else{
    p = qplot(x=c(1:partition),y=distance,geom="line",xlab="Partitions-Time Series",ylab="Distance") +
      ggtitle("Stochastic distance Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  print(p)
  #dev.off()
  return(distance)
}








