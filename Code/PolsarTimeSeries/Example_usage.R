source("SARTimeSerie.R")
source("Band&Pompe.R")
require(grid)
require(gridExtra)
require(ggsci)
require(extrafont)
require(ggpubr)

#The SAR data is available on https://drive.google.com/file/d/1jtbOcYwQfysfcUp4UhoA7lSl4_tPIqfa/view?usp=sharing and
# correspond to HHHH band of an image taken from the Cape Canaveral (acquired Sep 22, 2016)

#Ocean regions in Cape Canaveral
#{Behavior 1}
dim <- c(100, 200, 1700, 200)
dim <- c(300, 200, 1700, 200)
dim <- c(100, 200, 1900, 200)
dim <- c(300, 200, 1900, 200)
#{Behavior 2}
dim <- c(100, 200, 100, 200)
dim <- c(400, 200, 100, 200)
dim <- c(700, 200, 100, 200)
dim <- c(1100, 200, 100, 200)


#The SAR data is available on https://drive.google.com/file/d/1pO6p_UI9Cgdci9y6jVynAv8SrrAvv7K8/view?usp=sharing and
# correspond to HHHH band of an image taken from the Munich, Germany (acquired Jun 5, 2015) 

#Urban regions in Munich
dim <- c(3000, 200, 400, 200)
dim <- c(3000, 200, 600, 200)
dim <- c(3200, 200, 400, 200)
dim <- c(3200, 200, 600, 200)

ns <- 8
dimen <- matrix(nrow = ns, ncol = 4)

#Forest regions in Guatemala
dimen[1,] <- c(5600, 200, 2700, 200) #region 1
dimen[2,] <- c(5200, 200, 2800, 200) #region 2
dimen[3,] <- c(4100, 200, 2930, 200) #region 3
dimen[4,] <- c(1075, 200, 1930, 200) #region 4

#Crop regions in Guatemala
dimen[5,] <- c(400, 200, 1500, 200) #region 5

#Ground regions in Guatemala
#---------------------------------------------------
#Possibilly you will have problems with this regions
#It isn't uniform
dimen[6,] <- c(250, 200, 1100, 200) #region 6
dimen[7,] <- c(250, 200, 1850, 200) #region 7
dimen[8,] <- c(1675, 200, 1930, 200) #region 8

d1 <- dimen[1,2]
d2 <- dimen[1,4]
n <- c(2,3,4,5,6)
tal <- c(1,2,3,4,5)
a <- b <- 0
plots <- array(list(), 25)

#Get Time Serie from Guatemala SAR data

for(i in 1:25){
  if(i%%5 == 1){
    a <- a + 1
    b <- 0
    if(n[a] != 2){
      c1x = readingMPR(n[a],1)
      c1y = readingMPR(n[a],2)
      c2x = readingMPR(n[a],3)
      c2y = readingMPR(n[a],4)
      continua <- data.frame(c1x, c1y)
      trozos <- data.frame(c2x, c2y)
    }
  }
  b <- b + 1
  xy <- get.number.patterns(d1, d2, n[a], tal[b])
  probability <- matrix(nrow = ns, ncol = factorial(n[a]) + 1)
  my.elements <- matrix(nrow = xy, ncol = n[a]*n[a]) 
  
  for(j in c(1:ns)){
    get.serie <- array(0, dim = c(xy[1], n[a], n[a]))
    get.serie <- getTimeSerie("guatemala", dimen[j,], n[a], tal[b]) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
    for(w in 1:xy[1]){
      my.elements[w,] <- as.vector(t(get.serie[w,,]))
    }
    probability[j, 1:factorial(n[a])] <- Bandt.Pompe(as.vector(t(my.elements)), n[a], xy)
  }
  
  probability[1:4, factorial(n[a]) + 1] = 1
  probability[5, factorial(n[a]) + 1] = 2
  probability[6:8, factorial(n[a]) + 1] = 3
  
  plots[[i]] <- HCPlane(probability, ns, n[a]) + theme(axis.title.x=element_blank(),
                                                       axis.title.y=element_blank(), 
                                                       legend.position = "none")
}

ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
          plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]],
          plots[[11]], plots[[12]], plots[[13]], plots[[14]], plots[[15]],
          plots[[16]], plots[[17]], plots[[18]], plots[[19]], plots[[20]],
          plots[[21]], plots[[22]], plots[[23]], plots[[24]], plots[[25]],
          ncol=5, nrow=5, common.legend = TRUE, legend = "right") + 
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + labs(colour=expression(italic(Regions))) +
  theme_igray() + theme(text=element_text(size=14, family="Times New Roman"), axis.text.x=element_blank(), axis.text.y=element_blank()) + 
  guides(colour = guide_legend(override.aes = list(size=3)))
