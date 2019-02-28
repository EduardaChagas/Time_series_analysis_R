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

k <- c(0, .5, 1, 1.5, 2, 3)

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

