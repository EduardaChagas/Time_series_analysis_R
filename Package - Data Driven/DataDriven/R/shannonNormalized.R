#' @title Calculates the normalized Shannon entropy
#' @name shannonNormalized
#'
#' @description Returns the normalized Shannon entropy of a probability distribution
#'
#' @param p A numerical vector containing an ordinal pattern distribution.
#'
#' @return Entropy of Shannon.
#'
#' @author Eduarda Chagas
#'
#' set.seed(1234567890, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' prob = dataDriven(x = x, dimension = d, delay = del)
#' shannonNormalized(p = prob)
#'
#' @export
shannonNormalized <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  h <- (-sum(h))
  return(h/log(length(p)))
}
