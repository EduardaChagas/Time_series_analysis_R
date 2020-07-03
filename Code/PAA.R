library(ggplot2)

paa <- function(series, M){
  res = rep(0, M)
  N = length(series)
  for(i in 0:(N*(M - 1))){
    res[i%/%N + 1] = res[i%/%N + 1] + series[i%/%M + 1]
  }
  for(i in 1:M){
    res[i] = res[i]/N
  }
  res
}

plotPAA<-function(serie,size,option=0){
  vline=seq(from=0,to=length(serie),by=(length(serie)/size))
  segm = rep(0,size+1)
  segm[1:size] = paa(serie,size)
  segm[size+1] = segm[size]
  steps = data.frame(x=vline,y=segm)
  segm = as.double(format(round(segm,2),nsmall=2))
  myText = segm[1:size]
  #png("myPAA.png")
  p = qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie",colour="red") +
      ggtitle("Piecewise Aggregate Approximation") + theme(plot.title = element_text(hjust=0.5)) +
      geom_step(data=steps,aes(x=x,y=y),colour="black") +
      geom_text(aes(x=(vline[1:(length(vline)-1)]+vline[2:length(vline)])/2,y=segm[1:(length(segm)-1)]*1.05,label=myText,colour="blue"))
  print(p)
  #dev.off()
  return(segm[1:size])
}

sum_of_variation<-function(segment){
  c1 = segment[1:(length(segment)-1)]
  c2 = segment[2:length(segment)]
  res = sum(abs(c1-c2))
  res
}

ssv<-function(serie,size){
  aux = matrix(nrow=round(length(serie)/size),ncol=size)
  res = rep(0,round(length(serie)/size))
  ini = row = sum = i = 1
  while((i <= length(serie)) && (row <= round(length(serie)/size))){
    aux[row,sum] = serie[i]
    if(sum == size){
      row = row + 1
      sum = 1
      i = ini + size - 1
      ini = i
    }else{
      i = i + 1
      sum = sum + 1
    }
  }
  for(i in 1:round(length(serie)/size)){
    res[i] = sum_of_variation(aux[i,])
  }
  res
}

