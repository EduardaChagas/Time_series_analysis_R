require(fftw)
require(ggplot2)
require(ggthemes)
require(ggpubr)
require(reshape2)
require(ggrepel)
require(statip)
require(DataDriven)

atack <- function(ns,series,p,n){
  Series.to.analysis <- matrix(nrow = ns + 1, ncol = n)
  for(j in c(1:ns)){
    x <- series
    indicator <- rbern(n,p)
    for(i in c(2:n)){
      if(indicator[i]==1){ 
        x[i] = x[i-1]
      }
    }
    Series.to.analysis[j,] <- x
  }
  Series.to.analysis[ns+1,] <- series
  Series.to.analysis
}

series_generator <- function(pp,y,n,k){
  Series <- vector(mode="numeric")
  filtro <- (1:n)^-(k/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro    
  x1 <- IFFT(y1, plan=pp)  
  Series <- c(Re(x1)) 
  Series
}

hc_plane <- function(Series.to.analysis ,ns, dimension, delay){
  prob <- matrix(nrow = 4*ns, ncol = factorial(dimension))
  Complexity <- Entropy <- Dist <- rep(0,(4*ns))
  j <- 0
  zeros <- rep(0,factorial(dimension))
  
  patterns <- formationPattern(series,dimension,delay,0)
  elements <- formationPattern(series,dimension,delay,1)
  symbols <- definePatterns(dimension)
  
  for(i in c(1:ns)){
    prob[i + j,] <- completeCase(Series.to.analysis[i,], dimension, delay, patterns, elements, symbols) #Blue
    Entropy[i + j] <- shannonNormalized(prob[i + j,])
    Complexity[i + j] <- Ccomplexity(prob[i + j,])
    
    prob[i + j + 1,] <- timeOrdered(Series.to.analysis[i,], dimension, delay, p_patterns, symbols) #Green
    Entropy[i + j + 1] <- shannonNormalized(prob[i + j + 1,])
    Complexity[i + j + 1] <- Ccomplexity(prob[i + j + 1,])
    
    prob[i + j + 2,] <- randomImputation(Series.to.analysis[i,], dimension, delay, patterns, elements, symbols) #Orange
    Entropy[i + j + 2] <- shannonNormalized(prob[i + j + 2,])
    Complexity[i + j + 2] <- Ccomplexity(prob[i + j + 2,])
    
    #prob[i + j + 3,] <- dataDriven(Series.to.analysis[i,], dimension, delay) #Wine
    #Entropy[i + j + 3] <- shannonNormalized(prob[i + j + 3,])
    #Complexity[i + j + 3] <- Ccomplexity(prob[i + j + 3,])
    j <- j + 3
  }
  print(prob)
  DD <-rep(c(21:24),ns)
  Shapes <- rep(c(8,15,16,17),ns)
  Colors <- rep(c('#003366','#366501', '#ff9900', '#800000'),ns)
  Entropy.Complexity <- data.frame(Entropy, Complexity, Dist, Shapes, Colors)
  
  prob2 <- dataDriven(Series.to.analysis[ns+1,], dimension, delay) #Vinho
  Entropy2 = shannonNormalized(prob2)
  Complexity2 = Ccomplexity(prob2)
  
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(x=c2x,y=c2y,geom="line",xlab="Shannon Entropy",ylab="MPR Statistical Complexity") +
    ggtitle("Entropy-Complexity Plane") + theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c1x,c1y)) + 
    geom_point(aes(x = Entropy.Complexity$Entropy,y = Entropy.Complexity$Complexity), shape = Entropy.Complexity$Shape, color = Entropy.Complexity$Colors, size = 2) +
    geom_point(aes(x = Entropy2, y = Complexity2))
  
  print(p)
  Entropy.Complexity
}


# Generating the initial series
n <- 10^5
set.seed(seed = 1234567890, kind = "Mersenne-Twister")
x <- rnorm(n)
x <- x - mean(x)
pp <- planFFT(n)
y <- FFT(x, plan=pp)

k <- 2
p <- c(0.1,0.3,0.5,0.7)
series <- series_generator(pp,y,n,k)
Series.to.analysis <- atack(10,series,p,n)
dimension <- 3
delay <- 1
ns <-10
hc_plane(Series.to.analysis, ns, dimension, delay)
