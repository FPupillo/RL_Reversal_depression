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

for (fun in list.files("helper_functions")){
  
  source(paste0("helper_functions/", fun))
  
}

# get the simulating models
for (model in list.files("simulation_functions")){
  
  source(paste0("simulation_functions/",model))
  
}

#model<-Args<-commandArgs(trailingOnly = T) 

model<-"RW_2alpha_beta"
sim_model<-get(paste0("simulate_", model))

# simulate behaviour at different alpha values and beta
sims<-10

alphapos<-seq(0.1, 1, length.out =sims)
alphaneg<-seq(0.1, 1, length.out =sims)

beta<-seq(2, 2, length.out = sims)

# virtual part
part<-1:sims

# tget the data structure
data<-taskSim(Preward = 0.90, Ploss=0.90, Ntrials =30)


# detect cores for runnning in parallel
cores=detectCores()


cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

simul<-100

totsim<-sims*sims*simul

curr_sim<-1


# simulate data    
pb<-txtProgressBar(min=1, max= (simul), style = 3)

#for (simul in 1:sims){
#for (ap in alphapos){
#  for (an in alphaneg){
#for (simul in 1:100){
dat<-foreach (j=1:simul, .combine=rbind)  %dopar% {
  
  simData<-vector()
  
  for (ap in alphapos){
    for (an in alphaneg){
      
      sim<-sim_model(Data=data, alphapos = ap, alphaneg = an,
                     beta=beta[sims],
                     initialV = 0)
      
      # assign the values
      sim$alphapos<-ap
      sim$alphaneg<-an
      #sim$part<-sims
      
      
      
      simData<-rbind(simData, sim)
      
      part<-part+1
      
      curr_sim<-curr_sim+1
      
    }
  }
  
  setTxtProgressBar(pb, j) 
  
  write.csv(simData, paste0("temp/",j ,".csv"))
  
}


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
