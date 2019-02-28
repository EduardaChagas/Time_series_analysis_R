shannonEntropy <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonNormalized <- function(p){
  #shannon = round(shannonEntropy(p)/log(length(p)),precision)
  return(shannonEntropy(p)/log(length(p)))
}

tsallisEntropy <- function(p,q){  
  entropy = sum(p^q)
  entropy = (1 - entropy)/(q - 1)
  entropy
}

tsallisNormalized <- function(p,q){  
  ent_max = (1 - (length(p)^(1 - q)))/(q - 1)
  return(tsallisEntropy(p,q)/ent_max)
}

renyiEntropy <- function(p,q){
  entropy = sum(p^q)
  entropy = log(entropy)
  entropy = entropy/(1 - q)
  entropy
}

renyiNormalized <- function(p,q){ 
  return(renyiEntropy(p,q)/log(length(p)))
}
  
  
  
  
