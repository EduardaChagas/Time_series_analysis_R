library(combinat)
library(ggplot2)
library(dygraphs)
#library(plotly)

timeSeries<-function(serie){ 
  png("myplot.png")
  p = qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
    ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5))
  print(p)
  dev.off()
}

#Points == 0 -> first point
#Points != 0 -> all points
patternsOnGraph<-function(serie,dimension,delay,number_of_pattern,points = 0){
  lengthW=0
  point_time=point_value=c(1:length(serie))
  p_patterns <- formationPattern(serie,dimension,delay,0)
  n_symbols <- dim(p_patterns)[1]
  symbols <- definePatterns(dimension)
  elements <- formationPattern(serie,dimension,delay,1)
  index <- formationPattern(serie,dimension,delay,2)
  print(symbols[number_of_pattern,])
  for(i in 1:n_symbols){
    if(all(p_patterns[i,]==symbols[number_of_pattern,])){
      if(points==0){
        lengthW = lengthW + 1
        point_value[lengthW]=elements[i,1]
        point_time[lengthW]=index[i,1]
      }
      else{
        point_value[(lengthW+1):(lengthW+dimension)] = elements[i,1:dimension]
        point_time[(lengthW+1):(lengthW+dimension)] = index[i,1:dimension]
        lengthW = (lengthW+dimension)
      }
    }
  }
  png("myPattern.png")
  if(lengthW!=1){
    p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
    #p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Tempo",ylab="Série") +
      #ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5)) +
      ggtitle("Série Temporal") + theme(plot.title = element_text(hjust=0.5)) +
      geom_point(aes(x=point_time[1:lengthW],y=point_value[1:lengthW]),color="blue")
  }else{
    p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
    #p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Tempo",ylab="Série") +
      #ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5))
      ggtitle("Série Temporal") + theme(plot.title = element_text(hjust=0.5))
  }
  print(p)
  dev.off()
}


histogram<-function(serie, dimension, delay){
  fat=factorial(dimension)
  p_patterns <- formationPattern(serie,dimension,delay,0)
  n_symbols <- dim(p_patterns)[1]
  symbol <- definePatterns(dimension)
  index_rep=array(0,n_symbols)
  for(i in 1:n_symbols){
    for(j in 1:fat){
      if(all(p_patterns[i,]==symbol[j, ])){
        index_rep[i]=j
        break
      }
    }
  }
  index_rep=index_rep[1:n_symbols]
  index_rep = data.frame(i = index_rep)
  p <- ggplot(index_rep) +
      geom_histogram(aes(x = index_rep$i, y = ..density..),
                     binwidth = 1, fill = "grey", color = "black")+ 
      #labs(title="Histogram of the patterns", x="Patterns", y="Probability")
      labs(title="Histograma de Padrões", x="Padrões", y="Probabilidade")
  print(p)
  symbol = toString(symbol)
  return(symbol)
}

histogramImage<-function(myImg,dimx,dimy,delx,dely,dimension=0){  
  if(!dimension){
    dimension=dim(myImg)[1]
  }
  d = dimX*dimY
  fat = factorial(d)
  p_patterns <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension)
  n_symbols <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension, 1)
  symbols <- definePatterns(d)
  index_rep=array(0,n_symbols)
  for(i in 1:n_symbols){
    for(j in 1:fat){
      if(all(p_patterns[i,]==symbols[j, ])){
        index_rep[i]=j
        break
      }
    }
  }
  index_rep=index_rep[1:n_symbols]
  p = qplot(index_rep,geom="histogram",xlab="Patterns",ylab="Probability",binwidth=1) +
    ggtitle("Histogram of the patterns") + theme(plot.title = element_text(hjust=0.5))
  print(p)
  print(symbols)
}

