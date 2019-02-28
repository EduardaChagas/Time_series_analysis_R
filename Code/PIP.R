library(ggplot2)

PIP<-function(serie,numberPIPs){ 
  pip=result=rep(0,numberPIPs) 
  texto = c(1:numberPIPs)
  x1 = pip[1] = result[1] = 1
  x2 = pip[2] = result[2] = length(serie)
  y1 = serie[x1]
  y2 = serie[x2]
  initial = 1 
  n = 2 
  p = qplot(x=c(1:length(serie)),y=serie,xlab="Time",ylab="Serie",colour="blue") +
      ggtitle("Perceptually Important Points") + theme(plot.title = element_text(hjust=0.5)) +
      geom_line(aes(x=c(1:length(serie)),y=serie))
  for(i in 3:numberPIPs){
    pip[1:n] = sort(pip[1:n])
    dis = rep(0,length(serie))
    x1 = pip[initial]
    y1 = serie[x1]
    x2 = pip[initial+1]
    y2 = serie[x2]
    for(j in 1:(length(serie))){
      x3 = j
      y3 = serie[x3]
      dis[j] = abs((y1 + (y2 - y1)*((x3-x1)/(x2-x1))) - y3)
      if(dis[j] == max(dis)){
        pip[n+1] = j
      }
      if(j == x2){
        if(j != length(serie)){
          initial = initial + 1 
          x1 = pip[initial]
          y1 = serie[x1]
          x2 = pip[initial+1]
          y2 = serie[x2]
        }
      }
    }
    n = n + 1
    initial = 1
    result[n] = pip[n]
  }
  pip = sort(pip)
  #png("myPIP.png")
  p = p + geom_line(aes(x=pip,y=serie[pip]),colour="red") +
      geom_text(aes(x=pip,y=serie[pip]*1.1,label=texto,colour="black"))
  print(p)
  #dev.off()
  return(result)
}
