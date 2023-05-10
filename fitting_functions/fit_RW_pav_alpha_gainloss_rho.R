fit_RW_pav_alpha_gainloss_rho<-function(data, 
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
  
  X0<-c( runif(1),  runif(1), runif(1, min =rhoBound[1], max = rhoBound[2] ))  

  LB<-c(alphagainBound[1],alphalossBound[1], rhoBound[1]) # lower boundary
  UB<-c(alphagainBound[2], alphalossBound[2], rhoBound[2]) 

  
  obfunc<-function(x) likelihood_RW_pav_alpha_gainloss_rho(Data = data, 
                                                       alphagain = x[1] ,
                                                       alphaloss = x[2],
                                                       rho = x[3], 
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
  data <- list("alphagain" = NegLL[[1]][1], "alphaloss" = NegLL[[1]][2],
               'rho' = NegLL[[1]][3],
               "logLikel" = LL,"BIC"= BIC)

  return(data)
}
