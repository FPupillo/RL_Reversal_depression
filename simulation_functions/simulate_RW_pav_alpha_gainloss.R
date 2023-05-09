simulate_RW_pav_alpha_gainloss<-function (Data,alpha=NULL,  alphagain, 
                                     alphpaloss, rho = NULL,
                                     rhogain = NULL, rholoss = NULL, initialV,
                                     lengthToSwitch = 60){
  #------------------------------------------------------------------------------#
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  #
  # Input
  #   Data: data containing the structure of the task
  
  #   alpha: alpha parameter 
  #   initialV: value of the inital V
  #
  # Output:
  #   dastaframe with $response and $object_cat
  #------------------------------------------------------------------------------#
  
  # PRediction
  Data$prediction<-NA
  
  for (n in 1:2){
    
    # Initialize variables: Vs, the expected values
    Data[[paste("V", n, sep="")]]<-NA
    
  }

  # Delta, prediction error
  Data$Delta<-NA
  
  # reward
  Data$reward<-NA
  
  # expected values
  Data$currV<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2")

  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  V<-rep(initialV, 2)
  Data[1, Vindex]<-V

  # loop over trials
  for (t in 1:nrow(Data)){

    # reset the V in the middle
    if (t == lengthToSwitch+1){
      V<-rep(initialV, 2)
    }
    
    
    # if the probe is left, 1, else,2
    which_symb<-ifelse(Data$outcome_probe[t]=="symbol_left", 1,2)
    
    # current expected value
    Data$currV[t]<-V[which_symb]
      
    # make response according to softmax
    # here, we are assuming that the expected value correspond to the prob
    # or seeing a positive or a negative response
    # the higher the expected value of receiving a positive response,
    # the more participants will choose p
    Data$prediction[t]<-chooseBinomial(c(V[which_symb], 1-V[which_symb]))
    
    # get the observation as 1 if that category is present, and 0 if it is not
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
    updateVal<-update_RW(r = Data$reward[t], V =  Data$currV[t], alpha = alpha)
    
    # update V
    V[which_symb]<-updateVal$V

    # assign it to the dataset
    Data$Delta[t]<-updateVal$delta
    
    # assign values to the dataset
    Data[t, Vindex]<-V
    #Data[t, Pindex]<-p
    
  }
  
  return(Data)
  
}

