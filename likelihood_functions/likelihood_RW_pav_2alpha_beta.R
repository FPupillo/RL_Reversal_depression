likelihood_RW_pav_2alpha_beta<-function (Data,alpha=NULL,  alphapos, 
                                     alphaneg, beta=NULL, betapos = NULL, 
                                     betaneg = NULL, k=NULL, gamma =NULL, 
                                     c = NULL, discountk = NULL, rho = NULL,
                                     rhopos = NULL, rhoneg = NULL, print, initialV){
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
    # Ps (probabilities for each category's choice)
    #Data[[paste("P", n, sep="")]]<-NA
    
  }
  
  # probability for the choice that participants' made on a trial
  #Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # participants' response
  Data$response<-NA
  
  # reward
  Data$reward<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2")

  # current V
  Data$currV<-NA
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  #prob<-NA
  
  V<-rep(initialV, 2)
  Data[1, Vindex]<-V
  # get the highlighted 
  
  # likelihood
  Data$likel<-NA
  
  # loop over trials
  for (t in 1:nrow(Data)){

    # update choice probabilities using the softmax distribution
    #p<-softmax(V, beta)
    
    # make choice 
    #Data$response[t] <- chooseBinomial(p)
    
    # if the probe is left, 1, else,2
    which_symb<-ifelse(Data$outcome_probe[t]=="symbol_left", 1,2)
    
    Data$response[t]<-V[which_symb]
    
    Data$reward[t]<-ifelse(Data$outcome_probe[t] == 'symbol_left',
                     Data$outcome_symbol1[t], Data$outcome_symbol2[t])
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$switch_cond[t]=="reward" | Data$switch_cond[t]=="punTorew"){
      if ( Data$reward[t]>0){
        Data$reward[t]<-1
      } else {
        Data$reward[t]<-0
      }
    } else{
      if ( Data$reward[t]<0){
        Data$reward[t]<-0
      } else {
        Data$reward[t]<-1
      }  
      
    }
    
    # get the PE
    PE<-Data$rewar[t]- V[which_symb]
    
    if (PE<0){
      alpha = alphapos
    }else{
      alpha=alphaneg
    }
    
    # update values
    updateVal<-update_RW(r = Data$reward[t], V = Data$response[t], alpha = alpha)
    
    # update V
    V[which_symb]<-updateVal$V

    # assign it to the dataset
    Data$Delta[t]<-updateVal$delta
    
    # assign values to the dataset
    Data[t, Vindex]<-V
    Data$currV[t]<-V[which_symb]
    
  }
  
  # regress the estimated value on the actual value
  reg<-lm(Data$reward~Data$currV)
  
  NegLL<- -logLik(reg)
  
  if (print ==1){
    return(NegLL)
  } else if (print==2){
    return(Data)}
}

