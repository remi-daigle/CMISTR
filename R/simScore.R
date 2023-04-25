#' Simulate raw scores based on risk and uncertainty
#'
#' @param risk the risk score assigned by the assessor
#' @param certainty the certainty score assigned by the assessor
#' @param n the number of simulations for the calculation of the final score
#'
#' @return
#' @export
#'
#' @examples
#' scores <- simScore(risk=0,certainty=1)

##probs is an r document contianing possible risk and certainty score combinations and how likely each are to occur. 
#The probs document can be found in data file.
simScore <- function(risk,certainty,n=1000){
  p <- probs[probs$Risk==risk&probs$Certainty==certainty,]
  sample(x=p$Score, size=n, prob=p$Probability,replace=TRUE)
}
