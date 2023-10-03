cum_acc_window<-function(df, n = 5, var_name){
  #----------------------------------------------------------------------------#
  # Function that computes the cumulative accuracy within a sliding window
  # INPUT: df - dataframe
  #        n = number of trials to aggregate
  #        var_name - name of the variables with participants' responses
  # OUTPUT: dataframe with the probability of correct
  #----------------------------------------------------------------------------#

  # first, aggregated
  df$cum_acc_window<-NA
  for (i in 1:(nrow(df)-n)){

  df$cum_acc_window[i]<-mean(df[[var_name]][i:(i+n)])


  }
#
#   df$cuedCharacter<-as.character((df$cuedCharacter))
#
#     # first, select m5
#     df_m5<-df[df$cuedCharacter == "stimuli/m5.bmp",]
#
#     # now, m2
#     df_m2<-df[df$cuedCharacter == "stimuli/m2.bmp",]
#
#     df_m5$prob_opt_perChar<-NA
#     df_m2$prob_opt_perChar<-NA
#     for (i in 1:(nrow(df_m5)-n)){
#
#       df_m5$prob_corr_perChar[i]<-mean(df_m5[[var_name]][i:(i+n)])
#
#       df_m2$prob_corr_perChar[i]<-mean(df_m2[[var_name]][i:(i+n)])
#
#
#   }
#
#   # bind them
#   df<- rbind(df_m5, df_m2)
#
#   # sort
#   df<-df[order(df$trialN),]


  return(df)

}
