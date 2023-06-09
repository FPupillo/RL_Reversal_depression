fit_RW_pav_alpha_rho_gainloss<-function(data, 
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
  
  X0<-c( runif(1), runif(1, min = rhogainBound[1], max = rhogainBound[2]),
         runif(1, min = rholossBound[1], max = rholossBound[2]))

  LB<-c(alphaBound[1],rhogainBound[1], rholossBound[1]) # lower boundary
  UB<-c(alphaBound[2], rhogainBound[2], rholossBound[2]) 

  
  obfunc<-function(x) likelihood_RW_pav_alpha_rho_gainloss(Data = data, 
                                                       alpha = x[1] ,
                                                       rhogain = x[2],
                                                       rholoss = x[3], 
                                                    initialV=initialV, print = 1,
                                                    lengthToSwitch =64) # this function 
  
  # is similar to the MATLAB "handle" function
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), nrow(data), NegLL[[2]])
  
  # Prepare results for output
  data <- list("alpha" = NegLL[[1]][1], 
               'rhogain' = NegLL[[1]][2],'rholoss' = NegLL[[1]][3],
               "logLikel" = LL,"BIC"= BIC)

  return(data)
}
