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


#model<-Args<-commandArgs(trailingOnly = T) 

model<-"RW_pav_2alpha_beta"
sim_model<-get(paste0("simulate_", model))

# simulate behaviour at different alpha values and beta
simul<-30

alphapos<-seq(0.1, 1, length.out =simul)
alphaneg<-seq(0.1, 1, length.out =simul)

# shuffle them
alphapos<-sample(alphapos)
alphaneg<-sample(alphaneg)

beta<-seq(2, 2, length.out = sims)

alphaposBound<-c(0,1)
alphanegBound<-c(0,1)

# virtual part
part<-1:sims

Ntrials<-30

# tget the data structure
data1<-taskSim(Preward = 0.75, Ploss=0.75,  ord = "rewTopun",Ntrials =30)
data2<-taskSim(Preward = 0.75, Ploss=0.75,  ord = "punTorew",Ntrials =30)

# bind the two
#data<-rbind(data1, data2)

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
data<-rbind(data1, data2)

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

simul<-30

totsim<-sims*sims*simul

curr_sim<-1

# simulate data    
pb<-txtProgressBar(min=1, max= (simul), style = 3)

#for (simul in 1:sims){
#for (ap in alphapos){
#  for (an in alphaneg){
#for (simul in 1:100){
#dat<-foreach (j=1:simul, .combine=rbind)  %dopar% {
sim_all<-as.data.frame(matrix(NA, nrow = simul, ncol = 6))

for (s in 1:simul){
 
  simData<-vector()
  
  #for (ap in alphapos){
   # for (an in alphaneg){
      
      sim1<-sim_model(Data=data1, alphapos = alphapos[s], alphaneg = alphaneg[s],
                     beta=beta[s],
                     initialV = 0.5)
      
      sim2<-sim_model(Data=data2, alphapos = alphapos[s], alphaneg = alphaneg[s],
                      beta=beta[sims],
                      initialV = 0.5)
      
      # assign the values
      sim1$alphapos<-alphapos[s]
      sim1$alphaneg<-alphaneg[s]
      
      sim2$alphapos<-alphapos[s]
      sim2$alphaneg<-alphaneg[s]
      
      #sim$part<-sims
      
      #simData<-rbind(sim1, sim2)
      
      part<-part+1
      
      curr_sim<-curr_sim+1
      
      # now we need to fit
      fit1<-fit_RW_pav_2alpha_beta(sim1,alphaposBound = alphaposBound,
                                  alphanegBound = alphanegBound, 
                                  initialV=0.5)
      
      fit2<-fit_RW_pav_2alpha_beta(sim2,alphaposBound = alphaposBound,
                                   alphanegBound = alphanegBound, 
                                   initialV=0.5)
      
      fit1$alphapos<-fit1$alpha[1]
      fit1$alphaneg<-fit1$alpha[2]
      
      fit2$alphapos<-fit2$alpha[1]
      fit2$alphaneg<-fit2$alpha[2]
      
      sim_all[s, ]<-c(alphapos[s], alphaneg[s], fit1$alpha[1], fit1$alpha[2],
                      fit2$alpha[1], fit2$alpha[2] )
      
      }
  
# assign names
names(sim_all)<-c("sim_alphapos", "sim_alphaneg", "fit_alphapos_rew_to_pun",
                  "fit_alphaneg_rew_to_pun",  "fit_alphapos_pun_to_rews",
                  "fit_alphaneg_pun_to_rew")  

plotalphapos_rew_to_pun<-ggplot(sim_all, aes(x=sim_alphapos, y=fit_alphapos_rew_to_pun)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("U parameter")


filenames<-list.files("temp", full.names = T )       

alldata <- do.call(rbind, lapply(filenames, read.csv))

#do.call(rbind, lapply(filenames, file.remove))

# aggregate perf
simDataAgg<-alldata %>%
   filter(trialN<=90) %>%
  group_by(alphapos, alphaneg) %>%
  summarise(perReward = mean(reward))

# plot matrix
p1<-ggplot(data = simDataAgg, aes(x = alphaneg, y = alphapos, fill = perReward))+
  geom_tile()+
  #scale_fill_gradient(low="blue", high="yellow",name ="p(correct \nchoice)") + theme_bw()
  scale_fill_viridis(name ="p(correct \nchoice)") + theme_bw()

print(p1)
