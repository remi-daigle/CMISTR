#' Calculate the CMIST Score
#'
#' @param risks a vector containing the risk scores of the 17 CMIST questions
#' @param uncertainties a vector containing the uncertainty scores of the 17 CMIST questions
#'
#' @return
#' @export
#'
#' @examples
# set.seed(11)
# risks <- sample(x = c(0:3),size = 17,replace = TRUE)
# uncertainties <- sample(x = c(0:3),size = 17,replace = TRUE)
# score <- CMISTScore(risks,uncertainties)

CMISTScore <- function(risks,uncertainties){
  if(length(risks)!=17&length(uncertainties)!=17){
    warning("The 'risks' and 'uncertainties' should have exactly 17 values")
  }
  if(!all(unique(c(risks,uncertainties))%in%c(0:3))){
    warning("The 'risks' and 'uncertainties' vectors should only contain the integers 1, 2, and 3. Anything labeled as 0 will be removed when
            calulating the CMIST Score. Therefore, only use 0 for question that are excluded from your assessment.")
  }

#input data
  input <- data.frame(risks,uncertainties)%>% 
    dplyr::mutate(g=as.numeric(row.names(.)))

  
  input_noZero<-input%>%
    dplyr::filter(risks>=1 & uncertainties >=1)
  
g_qu<-unique(input_noZero$g) 
qu_matrix<-list()

for(g_ in g_qu){
  input_noZero_nog<- input_noZero[input_noZero$g==g_,]%>%
    dplyr::select(-g)
  
  qu_matrix[[g_]] = apply(input_noZero_nog,1,function(x){
    CMISTR::simScore(x[1],x[2])
  })
}

#remove nulls from lists
non.null.list <- lapply(qu_matrix, lapply, function(x)ifelse(is.null(x), NA, x))%>%
  list(g=list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))

#convert list to dataframe containing id and list
df<-as.data.frame(do.call(cbind, non.null.list))%>%
  tidyr::unnest(V1)%>%
  dplyr::mutate(g=as.numeric(g),
         V1=as.numeric(V1))

#rejoin dataframes
rawrisks<-dplyr::left_join(input, df, by="g")
rawrisks[rawrisks=="NULL"]<-NA

rawrisks_wider<-rawrisks%>%
  dplyr::select(g, V1)%>%
  tidyr::pivot_wider(names_from = g, values_from = V1, values_fn=list)%>%
  tidyr::unnest(cols=everything())

rawscores <- do.call("rbind",
                     apply(rawrisks_wider,1,function(x){
                       data.frame(likelihood=mean(x[1:8]),
                                  impact=mean(x[9:17]))
                     }))

rawscores$score <- rawscores$likelihood*rawscores$impact

# rawscores_like<-rawrisks%>%
#   dplyr::filter(g %in% c("1", "2", "3", "4", "5", "6", "7", "8"))%>%
#   dplyr::group_by(g)%>%
#   dplyr::summarise(likelihood=mean(V1, na.rm=T))%>%
#   dplyr::select(likelihood)
# rawscores_imp<-rawrisks%>%
#   dplyr::filter(g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17"))%>%
#   dplyr::group_by(g)%>%
#   dplyr::summarise(impact=mean(V1, na.rm=T))%>%
#   dplyr::select(impact)
# 
# rawscores<-merge(rawscores_like, rawscores_imp, by.x=0, by.y=0, all=T)
# rawscores[rawscores=="NaN"]<-NA
# rawscores<-rawscores%>%
#   dplyr::mutate(score= dplyr::case_when( likelihood != "NA" & impact != "NA"~ (likelihood*impact),
#                                   likelihood == "NA" | impact =="NA" ~ (((likelihooh*impact), na.rm=T)))
#   )
                  
  data.frame(CMIST_Score=mean(rawscores$score, na.rm=T),
             CMIST_Upper=quantile(rawscores$score,0.975, na.rm=T),
             CMIST_Lower=quantile(rawscores$score,0.025, na.rm=T),
             Likelihood_Score=mean(rawscores$likelihood, na.rm=T),
             Likelihood_Upper=quantile(rawscores$likelihood,0.975, na.rm=T),
             Likelihood_Lower=quantile(rawscores$likelihood,0.025, na.rm=T),
             Impact_Score=mean(rawscores$impact, na.rm=T),
             Impact_Upper=quantile(rawscores$impact,0.975, na.rm=T),
             Impact_Lower=quantile(rawscores$impact,0.025, na.rm=T),
             row.names = NULL)

}

# #old version
# input <- data.frame(risks,uncertainties)
# 
# rawrisks_old <- apply(input,1,function(x){
#   CMISTR::simScore(x[1],x[2])
# })
# 
# rawscores <- do.call("rbind",
#                      apply(rawrisks,1,function(x){
#                        data.frame(likelihood=mean(x[1:8]),
#                                   impact=mean(x[9:17]))
#                      }))
# 
# rawscores$score <- rawscores$likelihood*rawscores$impact
