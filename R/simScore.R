#' Title
#'
#' @param risk
#' @param certainty
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#' scores <- simScore(risk=2,certainty=1)
simScore <- function(risk,certainty,n=1000){
  p <- probs[probs$Risk==risk&probs$Certainty==certainty,]
  sample(x=p$Score, size=n, prob=p$Probability,replace=TRUE)
  print('something dumb')
}
