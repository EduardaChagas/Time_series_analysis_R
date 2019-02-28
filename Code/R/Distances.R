euclidianDistance<-function(p){
  return(sqrt(squaredDistance(p)))
}

squaredDistance<-function(p){
  cc = rep(1/length(p),length(p))
  distance = sum((p-cc)^2)
  return(distance)
}

manhattanDistance<-function(p){
  cc = rep(1/length(p),length(p))
  distance = sum(abs(p-cc))
  return(distance)
}

chebyshevDistance<-function(p){
  cc = rep(1/length(p),length(p))
  distance = abs(p - cc)
  return(max(distance))
}

kullbackDivergence<-function(p){
  cc = rep(1/length(p),length(p))
  distance = p * log(p/cc)
  distance[is.nan(distance)||is.infinite(distance)] <- 0
  return(sum(distance))
}

hellingerDistance<-function(p){
  cc = rep(1/length(p),length(p))
  distance = sum((sqrt(p)-sqrt(cc))^2)*0.5
  return(sqrt(distance))
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

woottersDistance<-function(p,q){
  cc = rep(1/length(p),length(p))
  distance = sum(sqrt(p*cc))
  distance = acos(distance)
  return(distance)
}

bhattacharyyaDistance<-function(p){
  cc = rep(1/length(p),length(p))
  distance = sum(sqrt(p*cc))
  distance = -log(distance)
  distance
}

harmonicMean<-function(p){
  cc = rep(1/length(p),length(p))
  harmonic = sum((p*cc)/(p+cc))
  harmonic[is.nan(harmonic)||is.infinite(harmonic)] <- 0
  return(2*harmonic)
}