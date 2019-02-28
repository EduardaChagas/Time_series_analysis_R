weigths<-function(series,dimension,delay){
  groups = formationPattern(series,dimension,delay,1)
  n_groups = dim(groups)[1]
  weigth = rep(0,n_groups)
  for(i in 1:n_groups){
    m = rep(mean(groups[i,]),dimension)
    weigth[i] = (sum((groups[i,]-m)^2))/dimension
  }
  weigth
}

WPE<-function(series,dimension,delay){
  weigth = weigths(series,dimension,delay)
  simbols = formationPattern(series,dimension,delay,0)
  n_elements = dim(simbols)[1]
  patterns = definePatterns(dimension)
  MyFrequency = MyWeigth = rep(0,factorial(dimension))
  for(i in 1:n_elements){
    for(j in 1:factorial(dimension)){ 
      if(all(simbols[i,] == patterns[j,])){
        MyFrequency[j] = MyFrequency[j] + 1
        MyWeigth[j] = MyWeigth[j] + weigth[i]
        break
      }
    }
  }
  MyWeigth = MyWeigth/MyFrequency
  MyWeigth[is.nan(MyWeigth)] = 0
  pbw = (MyFrequency/n_elements)*MyWeigth
  pbw
}

