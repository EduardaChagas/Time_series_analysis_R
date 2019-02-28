#' @title Calculates the percentage of repeated elements in a time series
#' @name equalitiesValues
#'
#' @description Analyzes and calculates the percentage of how many repeated elements a time series has.
#'
#' @param x A numeric vector (e.g. a time series)
#'
#' @return Percentage of repeated elements of a time series.
#'
#' @author Eduarda Chagas
#'
#' set.seed(1234567890, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' equalitiesValues(x = x)
#'
#' @export
equalitiesValues<-function(x){
  aux = duplicated(x)
  answer = length(aux[aux==TRUE])
  answer = (answer*100)/length(x)
  answer = format(round(answer, 3), nsmall = 3)
  return(paste(answer,"%"))
} 