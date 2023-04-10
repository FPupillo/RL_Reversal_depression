#------------------------------------------------------------------------------#
# function that simulates the task
#------------------------------------------------------------------------------#

taskSim<-function(
    # probability of reward for the rewarded symbol
    Preward =0.80,
    # probability of losses for the loss symbol
    Ploss = 0.80,
    # number of switches
    Nswitch = 2,
    # number of trial between switches 
    Ntrials =20
){
  
  # define the symbols
  symbols<-c("symbol_1", "symbol_2")
  
  # define the data frame
  df<-as.data.frame(matrix(NA, ncol = 3, nrow = Ntrials*(Nswitch+1)))

  df[,1]<-1:nrow(df)
  df[,2]<-symbols[1]
  df[,3]<-symbols[2]
  
names(df)<-c("trialN", "symbol_left", "symbol_right") 

# create the switch
df$switch_cond<-rep(c(1:(Nswitch+1)), each = Ntrials)

# create the reward for each symbol
df$reward_symbol1<-NA
df$reward_symbol2<-NA
df$optimal_response<-NA

# counter for the trials
counter_trial<-1

# invert the prob
probs<-matrix(ncol=4, nrow = (Nswitch+1))
for (n in 1:(Nswitch+1)){
  if (n%%2!=0){
    
    probs[n,]<-c(1,0, 0,1)
    
  } else
    
    probs[n,]<-c(0,1,1,0)
  
}

for (block in 1:(Nswitch+1)){
  

  df$reward_symbol1[counter_trial:(counter_trial+(Ntrials-1))]<-
    sample(probs[block,1:2],Ntrials, prob = c(Preward, 1-Preward),
                            replace = T)
  
  
  df$reward_symbol2[counter_trial:(counter_trial+(Ntrials-1))]<-
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
    
    