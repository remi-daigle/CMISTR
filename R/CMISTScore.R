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
    mutate(g=as.numeric(row.names(.)))
  
  #Assigningzeros a larger score
  #input[input==0]<-999
  
  input_noZero<-input%>%
    dplyr::filter(risks>=1 & uncertainties >=1)
  # input_noZero_nog<-input_noZero%>%
  #   dplyr::select(-g)
  
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
  unnest(V1)%>%
  mutate(g=as.numeric(g),
         V1=as.numeric(V1))

#rejoin dataframes
rawrisks<-left_join(input, df, by="g")
rawrisks[rawrisks=="NULL"]<-NA

# rawrisks<-input_noZero%>%
#   group_by(g)%>%
#   CMISTR::simScore(risk=risks,certainty=uncertainties)
# 
#   rawrisks <- apply(input_noZero_nog,1,function(x){
#     CMISTR::simScore(x[1],x[2])
#     })

# 
# test<-rawrisks%>%
#   mutate(ranges=cut(g, seq(1, 17, 7)))%>%
#   group_by(ranges)%>%
#   mutate(likelihood=mean(V1, na.rm=T),
#             impact=mean(V1, na.rm=T))%>%
#   as.data.frame()


# rawrisks<-rawrisks%>%
#   mutate(range=case_when(g%in% c("1", "2", "3", "4", "5", "6", "7", "8")~'likelihood',
# #                          g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17")~'impact'))
# 
# for(g_ in g_qu){
#   if(g_ %in% c("1", "2", "3", "4", "5", "6", "7", "8")){
#   rawscores <- rawrisks%>%
#     filter(g==g_)%>%
#     #group_by(g)%>%
#     summarise(likelihood=mean(V1, na.rm=T))
#   }else{
#     rawscores <- rawrisks%>%
#       filter(g==g_)%>%
#      # group_by(g)%>%
#       summarise(impact=mean(V1, na.rm=T))
#   }
# }

rawscores_like<-rawrisks%>%
  dplyr::filter(g %in% c("1", "2", "3", "4", "5", "6", "7", "8"))%>%
  dplyr::group_by(g)%>%
  dplyr::summarise(likelihood=mean(V1, na.rm=T))%>%
  dplyr::select(likelihood)
rawscores_imp<-rawrisks%>%
  dplyr::filter(g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17"))%>%
  dplyr::group_by(g)%>%
  dplyr::summarise(impact=mean(V1, na.rm=T))%>%
  dplyr::select(impact)

rawscores<-merge(rawscores_like, rawscores_imp, by.x=0, by.y=0, all=T)%>%
  mutate(score=likelihood*impact)
                   
                   
#                    impact=mean(V1[g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17")], na.rm=T)
#     # likelihood=case_when(g %in% c("1", "2", "3", "4", "5", "6", "7", "8")~mean(V1, na.rm=T)),
#     #         impact=case_when(g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17")~mean(V1, na.rm=T)
#                              
#   )
# 
# rawscores <- rawrisks%>%
#   #mutate(ranges=cut(g, seq(1, 17, 7)))%>%
#   group_by(g)%>%
#   summarise(likelihood=mean(V1[g %in% c("1", "2", "3", "4", "5", "6", "7", "8")], na.rm=T),
#          impact=mean(V1[g %in% c("9", "10", "11", "12", "13", "14", "15", "16", "17")], na.rm=T))%>%
#   ungroup()%>%
#   mutate(likelihood_final=mean(likelihood),
#          impact_final=mean(impact))%>%
#   as.data.frame()%>%
#   select(likelihood, impact, g)

  # rawscores <- do.call("rbind",
  #                      apply(rawrisks,1,function(x){
  #   data.frame(likelihood=((sum(x[1:8]))/length(input_noZero)),
  #              #impact=mean(x[9:17])
  #              impact=((sum(x[9:17]))/length(input_noZero))
  #              )
  # }))
  

  rawscores$score <- rawscores$likelihood*rawscores$impact
  

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
