likelihood_RW_pav_alphapos_alphaneg<-function (Data,alpha=NULL,  alphagain, 
                                            alphaloss, rho = NULL, rhogain, rholoss,
                                            print, initialV,
                                            lengthToSwitch = 60){
  #------------------------------------------------------------------------------#
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  #
  # Input
  #   Data: data containing the structure of the task
  
  #   alpha: alpha parameter 
  #   beta:  beta parameter
  #   initialV: value of the inital Q
  #
  # Output:
  #   dastaframe with $response and $object_cat
  #------------------------------------------------------------------------------#
  
  for (n in 1:2){
    
    # Initialize variables: Vs, the expected values
    Data[[paste("V", n, sep="")]]<-NA
    
  }
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # reward
  Data$reward<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2")
  
  # current V
  Data$currV<-NA
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  V<-rep(initialV, 2)
  Data[1, Vindex]<-V
  
  # loop over trials
  for (t in 1:(lengthToSwitch*2)){
    
    # reset the V in the middle
    if (t == lengthToSwitch+1 ){
      V<-rep(initialV, 2)
    }
    
    # if the probe is left, 1, else,2
    which_symb<-ifelse(Data$outcome_probe[t]=="symbol_left", 1,2)
    
    # expected value
    currV<-V[which_symb]
    
    Data$currV[t]<-V[which_symb]
    
    if (Data$switch_cond[t]=="reward" | Data$switch_cond[t]=="punTorew"){
      alpha<-alphagain
      if ( Data$outcome[t]>0){
        Data$reward[t]<-1
      } else {
        Data$reward[t]<-0
      }
    } else{
      alpha<-alphaloss
      if ( Data$outcome[t]<0){
        Data$reward[t]<-0
      } else {
        Data$reward[t]<-1
      }  
      
    }
    
    # update values
    updateVal<-update_RW(r = Data$reward[t], V =   Data$currV[t], alpha = alpha)
    
    # update V
    V[which_symb]<-updateVal$V
    
    # assign it to the dataset
    Data$Delta[t]<-updateVal$delta
    
    # assign values to the dataset
    Data[t+1, Vindex]<-V
    
  }
  
  # regress the estimated value on the actual value
  reg<-lm(Data$prediction~Data$currV)
  
  NegLL<- -logLik(reg)
  
  if (print ==1){
    return(NegLL)
  } else if (print==2){
    return(Data)}
}

