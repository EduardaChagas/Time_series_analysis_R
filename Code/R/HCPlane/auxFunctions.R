require(gtools)
require(ggplot2)
require(ggthemes)

readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "continuaN6.txt"
    trozo = "trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "continuaN24.txt"
    trozo = "trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "continuaN120.txt"
    trozo = "trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "continuaN720.txt"
    trozo = "trozosN720.txt"
  }
  curva1x = Readtxt2(continua,1) 
  if(option==1) return(curva1x)
  curva1y = Readtxt2(continua,2)
  if(option==2) return(curva1y)
  curva2x = Readtxt2(trozo,1)
  if(option==3) return(curva2x)
  curva2y = Readtxt2(trozo,2)
  if(option==4) return(curva2y)
}

# Partition indicates the number of parts in which we divide the series
partitionMPR<-function(serie,dimension,delay,partition){
  complexity = entropy = rep(0,partition)
  div = floor(length(serie)/partition)
  if(partition != 1){
    for(i in 1:partition){
      initial = ((i-1)*div)
      end = initial + div
      if(i == 1){
        initial = 1
        end = div
      }
      aux = serie[initial:end]
      probability = distribution(aux,dimension,delay)
      entropy[i] = shannonNormalized(probability)
      complexity[i] = Ccomplexity(probability)
    }
  }
  else{
    probability = distribution(serie,dimension,delay)
    entropy = shannonNormalized(probability)
    complexity = Ccomplexity(probability)
  }
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  #png("myHC.png")
  p = qplot(x=c2x,y=c2y,geom="line",xlab="Shannon Entropy",ylab="MPR Statistical Complexity") +
    ggtitle("Entropy-Complexity Plane") + theme(plot.title = element_text(hjust=0.5)) +
    #p = qplot(x=c2x,y=c2y,geom="line",xlab="Entropia de Shannon",ylab="Complexidade Estatística") +
    #ggtitle("Plano HC") + theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c1x,c1y)) + geom_point(aes(x=entropy,y=complexity),color="blue")
  print(p)
  #dev.off()
  data = matrix(nrow = partition, ncol = 2)
  data[,1] = entropy
  data[,2] = complexity
  return(data)
}


HCPlane <- function(probabilities, ns, dimension){
  
  Complexity <- Entropy <- Dist <- rep(0,(4*ns))
  j <- 0
  
  for(i in c(1:ns)){
    Entropy[i + j] = shannonNormalized(probabilities[i + j,])
    Complexity[i + j] = Ccomplexity(probabilities[i + j,])
    
    Entropy[i + j + 1] = shannonNormalized(probabilities[i + j + 1,])
    Complexity[i + j + 1] = Ccomplexity(probabilities[i + j + 1,])
    
    Entropy[i + j + 2] = shannonNormalized(probabilities[i + j + 2,])
    Complexity[i + j + 2] = Ccomplexity(probabilities[i + j + 2,])
    
    Entropy[i + j + 3] = shannonNormalized(probabilities[i + j + 3,])
    Complexity[i + j + 3] = Ccomplexity(probabilities[i + j + 3,])
    j <- j + 3
    
  }
  
  DD <-rep(c(21:24),ns)
  Shapes <- rep(c(8,15,16,17),ns)
  Colors <- rep(c('#003366','#366501', '#ff9900', '#800000'),ns)
  Entropy.Complexity <- data.frame(Entropy, Complexity, Dist, Shapes, Colors)
  
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(x=c2x,y=c2y,geom="line",xlab="Shannon Entropy",ylab="MPR Statistical Complexity") +
    ggtitle("Entropy-Complexity Plane") + theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c1x,c1y)) + 
    geom_point(aes(x = Entropy.Complexity$Entropy,y = Entropy.Complexity$Complexity), shape = Entropy.Complexity$Shape, color = Entropy.Complexity$Colors, size = 2) 
  
  print(p)
  Entropy.Complexity
}


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
      }
    }
  }
  return(probability/n_symbols)
}

shannonEntropy <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonNormalized <- function(p){
  #shannon = round(shannonEntropy(p)/log(length(p)),precision)
  return(shannonEntropy(p)/log(length(p)))
}

Ccomplexity<-function(p){
  cc <- jensenDivergence(p) * constant(p) * shannonNormalized(p)
  return(cc)
}

jensenDivergence<-function(p){
  cc = rep(1/length(p),length(p))
  s_p = shannonEntropy(p)
  s_q = shannonEntropy(cc)
  s_pq = shannonEntropy((p+cc)/2)
  divergence = sum(s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

constant <- function(p){
  k = (0.5)/length(p)
  a1 = (0.5 + k) * log(0.5 + k)
  a2 = (length(p) - 1) * k * log(k)
  a3 = (1 - 0.5) * log(length(p))
  b = -1/(a1 + a2 + a3)
  return(b)
}

Readtxt2<-function(name,column){
  data = read.table(name, stringsAsFactors=FALSE, fileEncoding="latin1")
  data = data[,column]
  if(mode(data)=="character"){
    data = type.convert(data)
  }
  data = na.omit(data)
  return(data)
}