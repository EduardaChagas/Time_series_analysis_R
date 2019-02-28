#' @title Determines a precise number of a set of numbers
#' @name precision
#'
#' @description Returns the numeric vector supplied with the given decimal number.
#'
#' @param x A numeric vector (e.g. a time series)
#' @param y Number of decimal places
#'
#' @return The vector \code{x} com \code{y} decimal places.
#'
#' @author Eduarda Chagas
#'
#' set.seed(1234567890, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' y <- 4
#' precision(x = x, y = y)
#'
#' @export
precision <-function(x, y){
  x = round(x*(10^y))
  x
}