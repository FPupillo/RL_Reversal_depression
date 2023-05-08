#------------------------------------------------------------------------------#
# function that simulates the task
#------------------------------------------------------------------------------#

taskSim<-function(
    # probability of reward for the rewarded symbol
    Preward =0.80,
    # probability of losses for the loss symbol
    Ploss = 0.80,
    # number of switches
    Nswitch = 1,
    # order switches
    ord = "rewTopun", # alternative is punTorew
    # number of trial between switches 
    Ntrials =30
){
  
  # define the symbols
  symbols<-c("symbol_1", "symbol_2")
  
  # define the data frame
  df<-as.data.frame(matrix(NA, ncol = 3, nrow = Ntrials*(Nswitch+1)))

  df[,1]<-1:nrow(df)
  df[,2]<-symbols[1]
  df[,3]<-symbols[2]
  
names(df)<-c("trialN", "symbol_left", "symbol_right") 

# # create the switch
# ord_both<-c("rewTopun", "punTorew")
# if(ord_both[1]==ord){

if(ord == "rewTopun"){  

df$switch_cond<-rep(c("reward", ord) , each = Ntrials)

}else{
  df$switch_cond<-rep(c("punishment", ord) , each = Ntrials)
}


# } else{
#   
#   df$switch_cond<-rep(c("punTorew", "rewTopun") , each = Ntrials)
#   
# }

# create the reward for each symbol
df$outcome_symbol1<-NA
df$outcome_symbol2<-NA
#df$outcome_response<-NA

# counter for the trials
counter_trial<-1

# invert the prob
probs<-matrix(ncol=4, nrow = (Nswitch+1))

rew_loss<-if (ord == "rewTopun"){c(1, -1)}else{ c(-1,1)}

for (n in 1:(Nswitch+1)){
  if (n%%2!=0){
    
    probs[n,]<-c(rew_loss[n], 0 ,0, rew_loss[n])
    
  } else
    
    probs[n,]<-c(rew_loss[n],0,0, rew_loss[n])
  
}

for (block in 1:(Nswitch+1)){
  

  df$outcome_symbol1[counter_trial:(counter_trial+(Ntrials-1))]<-
    sample(probs[block,1:2],Ntrials, prob = c(Preward, 1-Preward),
                            replace = T)
  
  
  df$outcome_symbol2[counter_trial:(counter_trial+(Ntrials-1))]<-
    sample(probs[block,3:4],Ntrials, prob = c(Preward, 1-Preward),
           replace = T)
  
  df$optimal_response[counter_trial:(counter_trial+(Ntrials-1))]<-
    which(probs[block,1:2]==max(probs[block,1:2]))
  
  counter_trial<-counter_trial+Ntrials
  
  
}

# create optimal choice symbol
df$optimal_symbol<-ifelse(df$optimal_response==1, "symbol_left", "symbol_right")

return(df)
  
}
    
    