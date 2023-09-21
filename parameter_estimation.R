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


for (model in list.files("likelihood_functions")){
  
  source(paste0("likelihood_functions/",model))
  
}

for (model in list.files("fitting_functions")){
  
  source(paste0("fitting_functions/",model))
  
}


model<-Args<-commandArgs(trailingOnly = T) 

model<-"RW_pav_alphapos_alphaneg"

fit_model<-get(paste0("fit_", model))

# get the directory were the data are
data_path<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk/data/"

files<-list.files(data_path, pattern = ".csv")

# delete the file with "participant" on it
files<- files[!substring(files, 1, 11)=="PARTICIPANT"]

# names of the parameters.
param<-c("alpha", "alphagain", "alphaloss", "rho", 'rhogain','rholoss' )

# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(files),ncol = length(param)+3 ) 

colnames(Parameters)<-c("PartNum", param,  "BIC", "LogLikel") #names of the columns

# create Bound
for (p in param){
  
  if (substr(p, 1, 5)=='alpha'){
    
    # boundaries
    assign(paste0(p, "Bound"), c(0,1))
  
}else if (substr(p, 1, 3) == "rho"){ 

  # boundaries
  assign(paste0(p, "Bound"), c(0,1))
  
}
  
}

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

#totsim<-sims*sims*simul

# simulate data    
#pb<-txtProgressBar(min=1, max= (simul), style = 3)

#for (simul in 1:sims){
#for (ap in alphapos){
#  for (an in alphaneg){
#for (simul in 1:100){
dat<-foreach (j=1:length(files), .combine=rbind)  %dopar% {
#sim_all<-as.data.frame(matrix(NA, nrow = simul, length(param)*2))
  tryCatch({
#for (s in 1:simul){
    
 
    print(paste0("working on participant ", j))
    
    # DEFINE PARAMETERs
    # delte parameters
    for (p in c("participant", "fitalpha", "fitalphagain", "fitalphaloss",
                "fitrho", "fitrhogain", "fitrholoss", "BIC", "LL")){
      assign((p),NA)
      
    }
    
    
    # read the file
    c_file<-read.csv(paste0(data_path,  files[j]))
    
    # select only rows of interest
    c_file<-c_file[nchar(c_file$Block_cond)>0,]
    
    # delete the practice
    c_file<-c_file[substr(c_file$Block_cond,1,8)!= "Practice",]
    
    # create the symbol highlighted
    c_file$symbol_highlighted<-ifelse(c_file$Highlighted=="right", 
                                      c_file$Symbol_right, 
                                      c_file$Symbol_left)
    
    #create the data outcome_proble
    c_file$outcome_probe<-ifelse(c_file$symbol_highlighted=="ｲ" |
                                   c_file$symbol_highlighted=="ﾌ"  , 
                                "symbol_left", "symbol_right")
    
    
    # create outcome
    c_file$outcome<-ifelse(c_file$Outcome == "nr" | c_file$Outcome == "nl", 0,
                           ifelse(c_file$Outcome== "r", 1, -1
                                  ))
    
   # now prediction
    c_file$prediction<-NA
    for (n in 1:nrow(c_file)){
      
      #if (c_file$switch_cond[n]=="reward" | c_file$switch_cond[n] == "revTopun"){
        
        if (c_file$key_resp_trial.keys[n]=="c"){
          
          c_file$prediction[n]<-1 # positive prediction
          
        } else if (c_file$key_resp_trial.keys[n]=="m"){
          
          c_file$prediction[n]<-0 # negative prediction
        } else{
          
          c_file$key_resp_trial.keys[n]<-NA
          
        }
      #} else {
        
       # if (c_file$key_resp_trial.keys[n]=="c"){
          
        #  c_file$prediction[n]<-0
          
        #} else {
        
         # c_file$prediction[n]<--1
          
      }
      

    # adapt the file for the parameter estimation script 
    # get the cond file
    c_file$switch_cond<-ifelse(c_file$Block_cond =="acquisition_P", "reward", 
                               ifelse(c_file$Block_cond =="reversal_P_N","revTopun",
                                      ifelse(c_file$Block_cond =="reversal_N_P", "punTorev", 
                                          ifelse(c_file$Block_cond =="acquisition_N", 'punishment', NA
                                          ))))
    
      
      # estimate likelihood and find the parameters of best fit
      fit<-fit_model(data=c_file, alphaBound = alphaBound, alphagainBound = alphagainBound,
                                  alphalossBound = alphalossBound, rhoBound = rhoBound,
                                  rhogainBound = rhogainBound, rholossBound = rholossBound, 
                                   initialV=0.5, lengthToSwitch = 32)

      for (par in c(param, "BIC", "LL")) {
        
        if (!is.null(fit[[par]])){
          
          assign(paste0("fit",par),  fit[[par]])
          
        } else{
          
          assign(paste0("fit",par), NA)
          
        }
      }
      
      participant<-unique(c_file$participant)
      
      BIC<- fit$BIC
      
      LL<-fit$logLikel
      
  }, error = function(e) { print(paste("problem with number", j,
                                       "ERROR:", conditionMessage(e)))})
  
      print(paste("completed est ", j))
      
      # get sub_n
 
      
      # sim_all[s, ]

      Parameters<-c( participant, fitalpha, fitalphagain, 
                     fitalphaloss,  fitrho, 
                     fitrhogain,  fitrholoss,  BIC, LL  )
      
      
      
     Parameters
     
     
}

stopCluster(cl)

colnames(dat)<-c("PartNum", param,  "BIC", "LogLikel") 

Parameters<-data.frame(dat)

# assign names

# write parameter estimation
write.csv(Parameters, paste0('output_folder/parameter_estimation_', 
  model, ".csv"), row.names = F)

# get