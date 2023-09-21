fit_RW_pav_alpha_alpharev<-function(data, 
                            alphaBound, 
                            alphagainBound,alphalossBound,
                            rhoBound, rhogainBound, rholossBound, initialV,
                           lengthToSwitch =60){
  
  #----------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood - pavlovian task
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<-a two-element vector with upper and lower boundaries for the 
  #                 alpha parameter 
  #    betaBound<- a two-element vector with upper and lower boundaries for the 
  #                 beta parameter 
  #    initialV<- inital Q values
  #
  # Output:
  #   A list with: 
  #   [[1]] "aalphagain, alphaloos" : parameters that minimize the 
  #           negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters 
  #           of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the 
  #         parameters of best fit
  #----------------------------------------------------------------------------#
  
  X0<-c( runif(1), runif(1))
  LB<-c(alphaBound[1], alphaBound[1]) # lower boundary
  UB<-c(alphaBound[2], alphaBound[2]) 

  
  obfunc<-function(x) likelihood_RW_pav_alpha_alpharev(Data = data, 
                                                       alpha = x[1] ,
                                                       alphagain = x[2],
                                                    initialV=initialV, print = 1,
                                              lengthToSwitch =64) # this function 
  
  # is similar to the MATLAB "handle" function
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # ntrials
  ntrials<-nrow(data[!is.na(data$prediction),])
  
  # compute BIC
  BIC <- BICcompute(length(X0), ntrials, NegLL[[2]])
  
  # Prepare results for output
  data <- list("alpha" = NegLL[[1]][1], 
               "alphagain" = NegLL[[1]][2],
               "logLikel" = LL,"BIC"= BIC)

  return(data)
}
