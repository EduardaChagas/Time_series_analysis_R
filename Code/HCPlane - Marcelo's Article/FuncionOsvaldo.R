### New function for Osvaldo
if(!require(fftw)) {
  installed.packages("fft2", dep=TRUE)
  require(fftw)
}


GeneraSeriek <- function(x, k) {
  
  ## Entradas
  # x: ruido blanco
  # k: el exponente de decrecimiento del espectro

  n <- length(x)  
  x <- x - mean(x)
  p <- planFFT(n)
  
  y <- FFT(x, plan=p)

  filtro <- (1:n)^-(k/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro     # Spectrum smoothing
  x1 <- IFFT(y1, plan=p)  # Back to time domain
  
  return(Re(x1))  
}

### Programa Principal

# Parámetros del estudio
set.seed(seed = 1234567890, kind = "Mersenne-Twister")
kk <- seq(0, 3.5, by=0.25)
n <- 10000

k <- 2.5
white_noise <- rnorm(n)
colored_noise <- GeneraSeriek(white_noise, k)

plot(colored_noise, type="l")

write.table(c(k, colored_noise), file="Serie2p5.txt", row.names = FALSE, col.names = FALSE)
