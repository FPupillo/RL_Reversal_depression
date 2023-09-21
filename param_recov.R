#------------------------------------------------------------------------------#
# simulate data with different models
# [1] "Fri Nov 18 17:38:39 2022"
#------------------------------------------------------------------------------#
rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)
library(foreach)
library(doParallel)
library(viridis)
library(gridExtra) # for plotting
library(ggpubr)

for (fun in list.files("helper_functions")){
  
  source(paste0("helper_functions/", fun))
  
}

# get the simulating models
for (model in list.files("simulation_functions")){
  
  source(paste0("simulation_functions/",model))
  
}


for (model in list.files("likelihood_functions")){
  
  source(paste0("likelihood_functions/",model))
  
}

for (model in list.files("fitting_functions")){
  
  source(paste0("fitting_functions/",model))
  
}


model<-Args<-commandArgs(trailingOnly = T) 

model<-"RW_pav_alpha_gainloss_rho_gainloss"

sim_model<-get(paste0("simulate_", model))
fit_model<-get(paste0("fit_", model))

# how many simulations?
simul<-100

# names of the parameters.
param<-c("alpha", "alphagain", "alphaloss", "rho", 'rhogain','rholoss' )

# create variables
for (p in param){
  
  if (substr(p, 1, 5)=='alpha'){
    
    # generate a range of variables
    c_p<-seq(0.1, 1, length.out=simul)
    
    # shuffle them
    assign(p, sample(c_p))
    
    # boundaries
    assign(paste0(p, "Bound"), c(0,1))
  
}else if (substr(p, 1, 3) == "rho"){ 
  
  # generate a range of variables
  c_p<-seq(0.1, 1, length.out=simul)
  
  # shuffle them
  assign(p, sample(c_p))
  
  # boundaries
  assign(paste0(p, "Bound"), c(0,1))
  
  
}
  
  # remove unnecessary variable
  rm(c_p)
  
}

# virtual part
part<-1:simul

# ntrials per block 
Ntrials<-30

set.seed(123235)
# tget the data structure
data1<-taskSim(Preward = 0.75, Ploss=0.75,  ord = "rewTopun",Ntrials =30)
data2<-taskSim(Preward = 0.75, Ploss=0.75,  ord = "punTorew",Ntrials =30)


# create for which symbol we show the outcome
data1$outcome_probe<-sample(c("symbol_left", "symbol_right"), Ntrials,
                            prob = c(0.5, 0.5), replace=T)
data2$outcome_probe<-sample(c("symbol_left", "symbol_right"), Ntrials,
                            prob = c(0.5, 0.5), replace = T)

# create the actual outcome
data1$outcome<-ifelse(data1$outcome_probe=="symbol_left", data1$outcome_symbol1,
                      data1$outcome_symbol2)
data2$outcome<-ifelse(data2$outcome_probe=="symbol_left", data2$outcome_symbol1,
                      data2$outcome_symbol2)

# bind the two
data_str<-rbind(data1, data2)

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

#totsim<-sims*sims*simul

# simulate data    
pb<-txtProgressBar(min=1, max= (simul), style = 3)

#for (simul in 1:sims){
#for (ap in alphapos){
#  for (an in alphaneg){
#for (simul in 1:100){
dat<-foreach (s=1:simul, .combine=rbind)  %dopar% {
#sim_all<-as.data.frame(matrix(NA, nrow = simul, length(param)*2))

#for (s in 1:simul){

      sim<-sim_model(Data=data_str,alpha= alpha[s], alphagain= alphagain[s], 
                     alphaloss = alphaloss[s], rho = rho[s], rhogain=rhogain[s],
                     rholoss = rholoss[s], initialV = 0.5)
      
      # select varof iterest
      sim<-sim[,c(1:11)]
      
      fit<-fit_model(data=sim, alphaBound = alphaBound, alphagainBound = alphagainBound,
                                  alphalossBound = alphalossBound, rhoBound = rhoBound,
                                  rhogainBound = rhogainBound, rholossBound = rholossBound, 
                                   initialV=0.5)

      for (par in param) {
        
        if (!is.null(fit[[par]])){
          
          assign(paste0("fit",par),  fit[[par]])
          
        } else{
          
          assign(paste0("fit",par), NA)
          
        }
      }
      
     
      print(paste("completed sim ", s))
      
      
      # sim_all[s, ]
     all_data<-c(alpha[s], fitalpha, alphagain[s], fitalphagain, 
                      alphaloss[s],  fitalphaloss, rho[s], fitrho, 
                      rhogain[s], fitrhogain, rholoss[s], fitrholoss  )
      
     all_data
     
      
}

stopCluster(cl)


sim_all<-data.frame(dat)

# assign names
names(sim_all)<-c( "sim_alpha", "fit_alpha", "sim_alphagain", "fit_alphagain",
                   "sim_alphaloss","fit_alphaloss", "sim_rho", "fit_rho", 
                   "sim_rhogain", "fit_rhogain", "sim_rholoss", "fit_rholoss")  

# get what are the parameters that are important
fitpar<-names(sim_all[1,])[substr(names(sim_all), 1, 3)=="fit"]

# parameters of interest
PoI<-fitpar[!is.na(sim_all[1,fitpar])]

# save the file
save( list=ls(),file=paste0("output_folder/param_recovery_", model, ".Rdata"))

