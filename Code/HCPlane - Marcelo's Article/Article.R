results <- read.csv("Data/results.csv")

results <- data.frame(results[,c(2,3,4,7,8,9)])

results$TN <- as.factor(results$TN)
results$D <- as.factor(results$D)
results$K <- as.factor(results$K)

require(ggplot2)
require(ggthemes)
require(grid)
require(gridExtra)
require(ggsci)
require(extrafont)
require(ggfortify)
require(ggpubr)
font_import()
loadfonts()

### Exploratory analysis

### Analisar variações
### Acrescentar curvas dos limites HxC quando cada "facet" seja para o mesmo D

readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "Data/continuaN6.txt"
    trozo = "Data/trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "Data/continuaN24.txt"
    trozo = "Data/trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "Data/continuaN120.txt"
    trozo = "Data/trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "Data/continuaN720.txt"
    trozo = "Data/trozosN720.txt"
  }
  curva1x = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==1) return(curva1x)
  curva1y = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==2) return(curva1y)
  curva2x = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==3) return(curva2x)
  curva2y = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==4) return(curva2y)
}

dimensions <- c(3,4,5,6)
TN <- c(1000, 10000, 50000, 1e+05, 5e+05)
plots <- array(list(), 20)
a <- b <- 0

for(i in 1:20){
  if(i%%5 == 1){
    a <- a + 1
    b <- 0
    c1x = readingMPR(dimensions[a],1)
    c1y = readingMPR(dimensions[a],2)
    c2x = readingMPR(dimensions[a],3)
    c2y = readingMPR(dimensions[a],4)
    continua <- data.frame(c1x, c1y)
    trozos <- data.frame(c2x, c2y)
  }
  b <- b + 1
  res <- results[(results$D == dimensions[a]) & (results$TN == TN[b]),]
  plots[[i]] <- ggplot(res, aes(x=H, y=C, col=K)) + geom_point(size=.7) + scale_x_continuous(limits=c(0.85, 1)) + scale_y_continuous(limits=c(0, 0.3)) +
    geom_line(data = continua, aes(x=c1x, y=c1y), color="gray") + geom_line(data = trozos, aes(x=c2x, c2y), color="gray")
  if((i %% 5 == 1) & (dimensions[a] != 6)){
    plots[[i]] <- plots[[i]] + theme(axis.text.x=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(), 
                                     legend.position = "none") 
  }else if(dimensions[a] == 6){
    plots[[i]] <- plots[[i]] + theme(axis.text.y=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(), 
                                     legend.position = "none")
    
  }else{
    plots[[i]] <- plots[[i]] + theme(axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.title.x=element_blank(), 
                                     axis.title.y=element_blank(), 
                                     legend.position = "none")
  }
  
}

ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
          plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]],
          plots[[11]], plots[[12]], plots[[13]], plots[[14]], plots[[15]],
          plots[[16]], plots[[17]], plots[[18]], plots[[19]], plots[[20]],
          ncol=5, nrow=4, common.legend = TRUE, legend = "right") + 
  xlab(expression(italic(H))) + ylab(expression(italic(C))) +   labs(colour=expression(italic(K))) +
  theme_igray() +   theme(text=element_text(size=14, family="Times New Roman")) + 
  guides(colour = guide_legend(override.aes = list(size=3)))



### NÃO LER A PARTIR DESTE PONTO
### PCA
autoplot(prcomp(df_D3_TN1000_K0[, c(4,5)]),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

### Regression

df_D3_TN1000_K0.lm <- lm(data=df_D3_TN1000_K0, C~H)
df_D3_TN1000_K0.lm
summary(df_D3_TN1000_K0.lm)