library(ggplot2)

lookupTable<-function(){
  lookup=unlist(read.table("lookupTable.txt"))
  matrix(lookup,nrow=9,byrow=T)
}

# O valor de numberSymbols varia de 3 a 10
breakpoints<-function(numberSymbols){
  numberSymbols = numberSymbols - 2
  lookup = lookupTable()
  na.omit(lookup[,numberSymbols])
}

zNormalization<-function(serie){
  (serie - mean(serie))/sd(serie)
}

saxPlot<-function(serie,numberSymbols,size){
  letters = c("a","b","c","d","e","f","g","h","j","k")
  sax = ""
  serie = zNormalization(serie)
  segm = rep(0,size+1)
  Psax = rep(0,size)
  segm[1:size] = paa(serie,size)
  segm[size+1] = segm[size]
  point = breakpoints(numberSymbols)
  for(i in 1:size){
    aux = 0
    for(j in 1:length(point)){ 
      if(segm[i] <= point[j]){
        sax = paste(letters[j],sax,sep="")
        Psax[i] = j
        aux = 1
        break
      }
    }
    if(!aux){ 
      sax = paste(letters[numberSymbols+1],sax,sep="")
      Psax[i] = numberSymbols+1
    }
  }
  steps = data.frame(x=vline,y=segm)
  #png("mySAX.png")
  p = qplot(geom="line",xlab="Time",ylab="Serie") +
    ggtitle("Symbolic Aggregate Approximation") + theme(plot.title = element_text(hjust=0.5)) +
    geom_hline(yintercept=point) + geom_step(data = steps,aes(x=x,y=y),colour="black") +
    geom_text(aes(x=(vline[1:(length(vline)-1)]+vline[2:length(vline)])/2,y=segm[1:(length(segm)-1)],label=letters[Psax],colour="red"))
  print(p)
  #dev.off()
  return(sax)
}