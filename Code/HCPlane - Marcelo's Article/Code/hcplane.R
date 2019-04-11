require(ggplot2)
require(ggthemes)
require(ggpubr)
require(reshape2)
require(ggrepel)
require(grid)
require(gridExtra)
require(fftw)
source('Aux.R')
source('plot.R')

# Paleta montada a partir de https://coolors.co/
rainbow_colors <- palette(c("#494947", #DarkGreen
                            "#7494EA", #MutedDarkBlue
                            "#B14AED", #Violet
                            "#44CCFF", #BrightLightBlue
                            "#35FF69", #BrightGreen
                            "#ED8438", #Orange
                            "#E7AD99", #Pink
                            "#C18C5D", #LightBrown
                            "#BF6F00", #DarkYellow
                            "#FB4D3D", #BrightRed
                            "#495867") #DarkGray
)

#Função geradora da série f^⁻k
series_generator_fk <- function(pp, y, n, k){
  Series <- vector(mode="numeric")
  filtro <- (1:n)^-(k/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro    
  x1 <- IFFT(y1, plan=pp)  
  Series <- c(Re(x1)) 
  Series
}

#Função geradora do mapa logístico
series_generator_map <- function(r, x_0, n){
  Series <- vector(mode="numeric",length = n)
  Series[1] <- x_0
  for(i in c(2:n)){
    Series[i] <- r*Series[i-1]*(1 - Series[i-1])
  }
  Series
}

Entropy.complexity.values <- function(series, dimension, delay, type, rainbow_colors){
  if(is.null(dim(series)[2])){ #Apenas uma série temporal
    distributions = distribution(series, dimension, delay)
    Entropy = shannonNormalized(distributions)
    Complexity = Ccomplexity(distributions)
  }
  else{ #Analisando mais de uma série temporal
    size = dim(series)[2] 
    Complexity <- Entropy <- rep(0,size)
    distributions <- matrix(nrow = factorial(dimension), ncol = size)
    for(i in 1:size){
      distributions[,i] = distribution(series[,i], dimension, delay)
      Entropy[i] = shannonNormalized(distributions[,i])
      Complexity[i] = Ccomplexity(distributions[,i])
    }
  }
  Shapes <- c(8,15,16,17)
  Entropy.Complexity <- data.frame(Entropy, Complexity, Shapes[type], rainbow_colors)
  Entropy.Complexity
}

#Plota as cotas do Plano HC
cotas <- function(dimension){
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(xlab=expression(H), ylab=expression(C)) +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c2x, y=c2y), size=2, color="gray") +
    geom_line(aes(x=c1x, c1y), size=2, color="gray")
  print(p)
  return(p)
}


#Vai adicionando os pontos de Entropia e Complexidade no Plano HC
###IMPORTANTE: ESTA FUNÇÃO É ACUMULATIVA, SEMPRE IRÁ ADIOCIONAR PONTOS. PARA REFAZER ALGUMA ADIÇÃO É NECESSÁRIO CHAMAR NOVAMENTE A FUNÇÃO "cotas(dimension)"
HCPlane <- function(p, Entropy.Complexity, dimension, want_dotted){
  if(want_dotted == 1 && dim(Entropy.Complexity)[1] > 1){
    init = 1
    end = dim(Entropy.Complexity)[1]
    p = p + 
      geom_segment(aes(x=Entropy.Complexity$Entropy[init:(end - 1)],
                       xend=Entropy.Complexity$Entropy[(init + 1):end],
                       y=Entropy.Complexity$Complexity[init:(end - 1)],
                       yend=Entropy.Complexity$Complexity[(init + 1):end]), linetype="dotted")
  }
  p = p +
    geom_point(aes(x = Entropy.Complexity$Entropy, y = Entropy.Complexity$Complexity), 
               shape = Entropy.Complexity$Shape, color = Entropy.Complexity$rainbow_colors, 
               size = 5)
  print(p)
  return(p)
}

#Função para a geração dos histogramas
histogram<-function(serie,dimension,delay,title){
  print(serie)
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
  print(p_patterns)
  index_rep = data.frame(i = index_rep)
  p <- ggplot(index_rep) +
    geom_histogram(aes(x = index_rep$i, y = ..density..),
                   binwidth = 1, fill = "grey", color = "black")+
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_x_continuous(limits=c(0, fat+2)) +
    labs(title=title, x="Patterns", y="Probability")
  return(p)
}

#Variáveis globais
n <- 10^4
dimension <- 6
delay <- 1
type <- rep(0,9) #Identificará que tipo de série estamos tratando (f^-k, mapa logistico...)

#f^-k com k: 0, 0.5, 1, 1.5, 2, 3
set.seed(seed = 1234567890, kind = "Mersenne-Twister")
x <- rnorm(n)
x <- x - mean(x)
pp <- planFFT(n)
y <- FFT(x, plan=pp)
k <- c(0, .5, 1, 1.5, 2, 2.5, 3)
series_fk <- matrix(nrow = n, ncol = length(k))
i <- 1
ii <- 1
for(kk in k) {
  series_fk[,i] <- series_generator_fk(pp, y, n, kk)
  type[ii] <- 1
  i <- i + 1
  ii <- ii + 1
}

#Mapa logístico: r= 4, 3.6 e x_0 = 0.1
r <- c(3.6,4)
i <- 1
x_0 <- 0.1
series_map <- matrix(nrow = n, ncol = length(r))
d2 <- matrix(nrow = factorial(dimension), ncol = length(r))
for(rr in r){
  series_map[,i] <- series_generator_map(rr,x_0,n)
  type[ii] <- 2
  i <- i + 1
  ii <- ii + 1
}

x <- seq(0, 2*pi, length.out=n)

#Sequência monotônica crescente log(x + .1)
series_monotonic <- log(x + 0.1)
type[ii] <- 3
ii <- ii + 1

#Sequência periódica sin(2x) * cos(2x)
series_periodic <- sin(2*x) * cos(2*x)
type[ii] <- 4

series <- data.frame(series_fk,series_map,series_monotonic,series_periodic)

#Plotando os histogramas das séries 
#h0 <- histogram(series_fk[,1],dimension,delay,"White Noise")
#h05 <- histogram(series_fk[,2],dimension,delay,expression(f^{-1/2}))
#h1 <- histogram(series_fk[,3],dimension,delay,expression(f^{-1}))
#h15 <- histogram(series_fk[,4],dimension,delay,expression(f^{-3/2}))
#h2 <- histogram(series_fk[,5],dimension,delay,expression(f^{-2}))
#h25 <- histogram(series_fk[,6],dimension,delay,expression(f^{-5/2}))
#h3 <- histogram(series_fk[,7],dimension,delay,expression(f^{-3}))
#hlogistic36 <- histogram(series_map[,1],dimension,delay,"Logistic Map r = 3.6")
#hlogistic4 <- histogram(series_map[,2],dimension,delay,"Logistic Map r = 4")
#hlog <- histogram(series_monotonic,dimension,delay,expression(paste("log(", x + .1, ")")))
#hsincos <- histogram(series_periodic,dimension,delay,expression(paste("sin(", 2 * x, ")", "cos(", 2 * x, ")")))

#Plotando todos os gráficos gerados em um grid
#grid.newpage()
#pushViewport(viewport(layout = grid.layout(3, 4)))
#vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#print(hlogistic36, vp = vplayout(1, 1)) 
#print(hlogistic4, vp = vplayout(1, 2)) 
#print(h3, vp = vplayout(1, 3))
#print(h25, vp = vplayout(1, 4))
#print(h2, vp = vplayout(2, 1)) 
#print(h15, vp = vplayout(2, 2)) 
#print(h1, vp = vplayout(2, 3)) 
#print(hlog, vp = vplayout(2, 4)) 
#print(hsincos, vp = vplayout(3, 1)) 
#print(h0, vp = vplayout(3, 2))
#print(h05, vp = vplayout(3, 3))

#Gerando os gráficos
subx <- 4500:5500
subx1 <- 4700:4900
subx2 <- 1:100*100

rainbow_colors <- palette(rainbow(11))

#Calculando os valores de entropia e complexidade
Entropy.Complexity.fk <- Entropy.complexity.values(series_fk, dimension, delay, type[1:7], rainbow_colors[1:7])
Entropy.Complexity.map <- Entropy.complexity.values(series_map, dimension, delay, type[8:9], rainbow_colors[8:9])
Entropy.Complexity.monotonic <- Entropy.complexity.values(series_monotonic, dimension, delay, type[10], rainbow_colors[10])
Entropy.Complexity.periodic <- Entropy.complexity.values(series_periodic, dimension, delay, type[11], rainbow_colors[11])

#Plotando o gráfico HCPlane
p <- cotas(dimension)
pHC <- HCPlane(p, Entropy.Complexity.fk, dimension,1)
pHC <- HCPlane(pHC, Entropy.Complexity.map, dimension,0)
pHC <- HCPlane(pHC, Entropy.Complexity.monotonic, dimension,0)
pHC <- HCPlane(pHC, Entropy.Complexity.periodic, dimension,0)

p0 <- qplot(x=subx, y=series_fk[subx,1], geom="line", color=I(rainbow_colors[1]), xlab="n", ylab="") +
  ggtitle("White Noise") + theme(plot.title = element_text(hjust=0.5))

p05 <- qplot(x=subx, y=series_fk[subx,2], geom="line", xlab="n", ylab="", color=I(rainbow_colors[2])) +
  ggtitle(expression(f^{-1/2})) + theme(plot.title = element_text(hjust=0.5))

p1 <- qplot(x=subx, y=series_fk[subx,3], geom="line", xlab="n", ylab="", color=I(rainbow_colors[3])) +
  ggtitle(expression(f^{-1})) + theme(plot.title = element_text(hjust=0.5))

p15 <- qplot(x=subx, y=series_fk[subx,4], geom="line", xlab="n", ylab="", color=I(rainbow_colors[4])) +
  ggtitle(expression(f^{-3/2})) + theme(plot.title = element_text(hjust=0.5))

p2 <- qplot(x=subx, y=series_fk[subx,5], geom="line", xlab="n", ylab="", color=I(rainbow_colors[5])) +
  ggtitle(expression(f^{-2})) + theme(plot.title = element_text(hjust=0.5))

p25 <- qplot(x=subx, y=series_fk[subx,6], geom="line", xlab="n", ylab="", color=I(rainbow_colors[6])) +
  ggtitle(expression(f^{-5/2})) + theme(plot.title = element_text(hjust=0.5))

p3 <- qplot(x=subx, y=series_fk[subx,7], geom="line", xlab="n", ylab="", color=I(rainbow_colors[7])) +
  ggtitle(expression(f^{-3})) + theme(plot.title = element_text(hjust=0.5))

plogistic36 <- qplot(x=subx1, y=series_map[subx1,1], geom="line", xlab="n", ylab="", color=I(rainbow_colors[8])) +
  ggtitle("Logistic Map r = 3.6") + theme(plot.title = element_text(hjust=0.5))

plogistic4 <- qplot(x=subx1, y=series_map[subx1,2], geom="line" ,xlab="n", ylab="", color=I(rainbow_colors[9])) +
  ggtitle("Logistic Map r = 4") + theme(plot.title = element_text(hjust=0.5))

plog <- qplot(x=subx2, y=series_monotonic[subx2], geom="line", xlab="x", ylab="", color=I(rainbow_colors[10])) +
  ggtitle(expression(paste("log(", x + .1, ")"))) + 
  theme(plot.title = element_text(hjust=0.5))

psincos <- qplot(x=subx2, y=series_periodic[subx2], geom="line", xlab="x", ylab="", color=I(rainbow_colors[11])) +
  ggtitle(expression(paste("sin(", 2 * x, ")", "cos(", 2 * x, ")"))) + 
  theme(plot.title = element_text(hjust=0.5))


#Plotando todos os gráficos gerados em um grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 4)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(pHC, vp = vplayout(2:4, 1:3))
print(plogistic36, vp = vplayout(1, 1)) 
print(plogistic4, vp = vplayout(1, 2)) 
print(p3, vp = vplayout(1, 3))
print(p25, vp = vplayout(1, 4))
print(p2, vp = vplayout(2, 4)) 
print(p15, vp = vplayout(3, 4)) 
print(p1, vp = vplayout(4, 4)) 
print(plog, vp = vplayout(5, 1)) 
print(psincos, vp = vplayout(5, 2)) 
print(p0, vp = vplayout(5, 3))
print(p05, vp = vplayout(5, 4))

