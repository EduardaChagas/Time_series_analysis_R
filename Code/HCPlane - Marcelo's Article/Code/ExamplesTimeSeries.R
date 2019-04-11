require(fftw)
require(ggplot2)
require(ggthemes)
require(ggpubr)
require(reshape2)
require(ggrepel)

n <- 10^4
set.seed(seed = 1234567890, kind = "Mersenne-Twister")
x <- rnorm(n)
x <- x - mean(x)

p <- planFFT(n)
y <- FFT(x, plan=p)

k <- c(0, .5, 1, 1.5, 2, 2.5, 3)

Series <- Spectrum <- vector(mode="numeric")
Power <- vector(mode="character")

for(kk in k) {
  
  filtro <- (1:n)^-(kk/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro     # Spectrum smoothing
  x1 <- IFFT(y1, plan=p)  # Back to time domain

  Series <- c(Series, Re(x1))
  Spectrum <- c(Spectrum, Mod(y1))
  Power <- c(Power, rep(kk, n))  

}

Series.and.PowerSpectra <- data.frame(Series, Spectrum, Power=as.factor(Power))

S <- ggplot(data=Series.and.PowerSpectra, aes(x=rep(1:n, length(k)), y=Series), group=Power) +
  geom_line() + 
  xlab("Time") + ylab("Time Series") +
  facet_grid(Power ~., scales="free") +
  theme_economist()

P <- ggplot(data=Series.and.PowerSpectra, aes(x=rep(1:n, length(k)), y=Spectrum), group=Power) +
  geom_line() + scale_y_log10() + 
  xlab("Frequency") + ylab("Power Spectrum") +
  facet_grid(Power ~.) +
  theme_economist()

ggarrange(S, P)


### Ilustra Bandt-Pompe

y <- c(1.8, 1.2, 3.2, 4.8, 4.2, 4.5, 2.3, 3.7, 1.2, .5)
Time.Series <- data.frame(x=1:10, y=y)

ggplot(data=Time.Series, aes(x=x, y=y)) +
  geom_line(col="gray") +
  geom_point(size=3.3, col="gray") +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(breaks=y[3:7], labels=NULL) +
  xlab(expression(italic(t))) +
  ylab(expression(italic(x[t]))) +
  geom_path(data=Time.Series[3:7,], 
            aes(x=x, y=y), lwd=1.3) + 
  geom_path(data=Time.Series[seq(1,9,by=2),], 
            aes(x=x, y=y), lwd=1.3, lty=4) + 
  geom_point(data=Time.Series[c(3:7,1,9),], 
             aes(x=x, y=y), size=3.3, col="black") +
  geom_label_repel(data=Time.Series[c(3:7,1,9),],
                   aes(x, y, label = y), size = 3.5, 
                   seed=12345678) +
  geom_segment(data = Time.Series[3:7,],
               aes(x = rep(0, 5), xend = x,
               y=y, yend=y),
               linetype=2,
               size=.2, 
                 arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
#  geom_segment(data = Time.Series[3:7,],
#               aes(x=x, xend=x, y=y, yend=rep(0,5),
#                   linetype=2,
#                   size=.2)
#               ) +
  annotate("text", label="pi[5]", x=-.5, y = y[4], parse=TRUE) +
  annotate("text", label="pi[4]", x=-.5, y = y[6], parse=TRUE) +
  annotate("text", label="pi[3]", x=-.5, y = y[5], parse=TRUE) +
  annotate("text", label="pi[2]", x=-.5, y = y[3], parse=TRUE) +
  annotate("text", label="pi[1]", x=-.5, y = y[7], parse=TRUE) +
  theme_classic()
ggsave(file="../Figures/IntroBP.pdf", width=18, height=15, units="cm")


