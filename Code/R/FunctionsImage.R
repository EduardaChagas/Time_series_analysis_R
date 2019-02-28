source(file.path('Functions.R'))

#Function to find the gray scale matriz of an image
#Function used in formationPatternsImage()
transformMatrix <-function(myImg,dimension){
  myMatrix <- matrix(nrow=dimension,ncol=dimension)
  for(i in 1:dimension){
    for(j in 1:dimension){
      myMatrix[i,j] = (0.3*myImg[i,j,1])+(0.59*myImg[i,j,2])+(0.11*myImg[i,j,3])
    }
  }
  return(myMatrix)
}

#Function to formation of image patterns 
#Option = 1 -> return n_symbols
formationPatternsImage <-function(myImg,dimX,dimY,delX,delY,dimension=0,option=0){
  if(!dimension){
    dimension=dim(myImg)[1]
  }
  n_symbols = previusX = 0
  d = dimX*dimY
  myArray = orderm = array(0,d)
  p_patterns = matrix(nrow=dimension*dimension,ncol=dimX*dimY)
  grayScale = transformMatrix(myImg,dimension)
  for(i in 1:dimension){
    if(i+(delY*(dimY-1)) <= dimension){
      for(j in 1:dimension){
        if(j+(delX*(dimX-1)) <= dimension){ 
          x = j
          y = i
          var = 0
          for(a in 1:dimY){
            if(a > 1){
              y = y + delY
            }
            for(b in 1:dimX){
              if(b > 1){
                x = x +delX
              }else{
                previusX = x
              }
              var = var + 1
              myArray[var] = grayScale[y,x]
            }
            x = previusX
          }
          n_symbols = n_symbols + 1
          p_patterns[n_symbols,] = order(myArray)
        }else{
          break
        }
      }
    }else{
      break
    }
  }
  p_patterns=p_patterns[1:n_symbols,]
  if(!option){
    return(p_patterns)
  }else{
    return(n_symbols)
  }
}

  distributionImage<-function(myImg,dimX,dimY,delX,delY,dimension=0,option=1){  
    if(!dimension){
      dimension=dim(myImg)[1]
    }
    d = dimX*dimY
    fat = factorial(d)
    f_absolute = probability = rep(0,fat)  
    p_patterns <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension)
    n_symbols <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension, 1)
    symbols <- definePatterns(d)
    aux = rep(0,n_symbols)  
    for(i in 1:fat){
      for(j in 1:n_symbols){
        if(aux[j] == 0){
          if(all(p_patterns[j,] == symbols[i,])){ 
            f_absolute[i]=f_absolute[i]+1
            aux[j]=1
          }
        }
      }
    }
    probability = f_absolute/n_symbols
    if(!option){
      return(f_absolute)
    }else{
      return(probability)
    }
  }
