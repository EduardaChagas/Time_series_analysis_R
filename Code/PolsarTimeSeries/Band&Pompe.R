require(ggplot2)
require(ggthemes)

Bandt.Pompe <- function(elements, dimension, elementsize){
  dyn.load("BandtPompe.so")
  probability <- .Call("BandtPompe", elements, dimension, elementsize)
  return (probability)
}

shannonEntropy <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonNormalized <- function(p){
  return(shannonEntropy(p)/log(length(p)))
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

Ccomplexity<-function(p){
  cc <- jensenDivergence(p) * constant(p) * shannonNormalized(p)
  return(cc)
}

HCPlane <- function(probabilities, ns, dimension){
  
  C <- H <- rep(0,ns)
  fact <- factorial(dimension)
  shape.select <- c(17,18,19)
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors <- palette(c("#494947", #DarkGreen
                              "#7494EA", #MutedDarkBlue
                              "#B14AED", #Violet
                              "#44CCFF", #BrightLightBlue
                              "#35FF69", #BrightGreen
                              "#ED8438", #Orange
                              "#E7AD99", #Pink
                              "#C18C5D", #LightBrown
                              "#BF6F00", #DarkYellow
                              "#FB4D3D", #BrightRed
                              "#495867")) #DarkGray
  
  for(i in c(1:ns)){
    H[i] = shannonNormalized(probabilities[i,1:fact])
    C[i] = Ccomplexity(probabilities[i,1:fact])
  }
  
  Entropy.Complexity <- data.frame(H, C, Regions = probabilities[,fact+1])
  
  p = ggplot(Entropy.Complexity, aes(x = H,y = C, col = Regions)) +
    theme(plot.title = element_text(hjust=0.5)) + geom_point(size=2)
  return(p)
}

get.number.patterns <- function(nrows, ncols, dimension, delay){
  if(dimension > delay){
    x <- 1 + (nrows - dimension) %/% (dimension - delay)
    y <- 1 + (ncols - dimension) %/% (dimension - delay)
  } else {
    x <- nrows %/% delay
    if(nrows %% delay >= dimension){
      x <- x+1
    }
    y <- ncols %/% delay
    if(ncols %% delay >= dimension){
      y <- y + 1
    }
  }
  return(x*y)
}

readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "Data/continuaN6.txt"
    trozo = "Data/trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "Data/continuaN24.txt"
    trozo = "Data/trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "Data/continuaN120.txt"
    trozo = "Data/trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "Data/continuaN720.txt"
    trozo = "Data/trozosN720.txt"
  }
  curva1x = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==1) return(curva1x)
  curva1y = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==2) return(curva1y)
  curva2x = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==3) return(curva2x)
  curva2y = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==4) return(curva2y)
}