fit_RW_pav_2alpha_beta<-function(data,alphaposBound,alphanegBound, initialV){
  
  #----------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<-a two-element vector with upper and lower boundaries for the 
  #                 alpha parameter 
  #    betaBound<- a two-element vector with upper and lower boundaries for the 
  #                 beta parameter 
  #    initialQ<- inital Q values
  #
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2]parameters that minimize the 
  #           negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters 
  #           of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the 
  #         parameters of best fit
  #----------------------------------------------------------------------------#
  
  X0<-c( runif(1),  runif(1))  # rexp generates random numbers from the exponential 
  #  distributon with mean 1
  LB<-c(alphaposBound[1],alphanegBound[1]) # lower boundary
  UB<-c(alphaposBound[2], alphanegBound[2]) 

  
  obfunc<-function(x) likelihood_RW_pav_2alpha_beta(data, alphapos = x[1] , alphaneg = x[2],
                                                    initialV=0.5, print = 1) # this function 
  
  # is similar to the MATLAB "handle" function
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), length(data$scene_cat), NegLL[[2]])
  
  # Prepare results for output
  data <- list(NegLL[[1]], LL, BIC)
  names(data)<-c("alpha", "logLikel", "BIC" )
  
  return(data)
}
