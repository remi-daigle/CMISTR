#' Calculate the CMIST Score
#'
#' @param risks a vector containing the risk scores of the 17 CMIST questions
#' @param uncertainties a vector containing the uncertainty scores of the 17 CMIST questions
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(11)
#' risks <- sample(x = c(1:3),size = 17,replace = TRUE)
#' uncertainties <- sample(x = c(1:3),size = 17,replace = TRUE)
#' score <- CMISTScore(risks,uncertainties)

CMISTScore <- function(risks,uncertainties){
  if(length(risks)!=17&length(uncertainties)!=17){
    stop("The 'risks' and 'uncertainties' must have exactly 17 values")
  }
  if(!all(unique(c(risks,uncertainties))%in%c(1:3))){
    stop("The 'risks' and 'uncertainties' vectors must only contain the integers 1, 2, and 3")
  }


  input <- data.frame(risks,uncertainties)

  rawrisks <- apply(input,1,function(x){
    CMISTR::simScore(x[1],x[2])
    })

  rawscores <- do.call("rbind",
                       apply(rawrisks,1,function(x){
    data.frame(likelihood=mean(x[1:8]),
               impact=mean(x[9:17]))
  }))

  rawscores$score <- rawscores$likelihood*rawscores$impact

  data.frame(CMIST_Score=mean(rawscores$score),
             CMIST_Upper=quantile(rawscores$score,0.975),
             CMIST_Lower=quantile(rawscores$score,0.025),
             Likelihood_Score=mean(rawscores$likelihood),
             Likelihood_Upper=quantile(rawscores$likelihood,0.975),
             Likelihood_Lower=quantile(rawscores$likelihood,0.025),
             Impact_Score=mean(rawscores$impact),
             Impact_Upper=quantile(rawscores$impact,0.975),
             Impact_Lower=quantile(rawscores$impact,0.025),
             row.names = NULL)

}
