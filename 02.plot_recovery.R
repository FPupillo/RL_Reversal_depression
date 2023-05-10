#------------------------------------------------------------------------------#
# file to plot the recovery
# 
# 
#------------------------------------------------------------------------------#
rm(list=ls())

library(ggplot2)
library(ggpubr)

# get the path
abs_path<-getwd()

# get the files
param_recov<-list.files(paste0(abs_path, "/output_folder"), 
                         pattern = "^param_recovery.") 

for (i in param_recov){
  
  # remove previous files
  #rm("parameter_recov")
  
 # load the files
  load(paste0(abs_path, "/output_folder/",  i))
  
  # get the model name
  model_name<-substr(i, 16, nchar(i)-6)
  
  # get how many param there are
  # get what are the parameters that are important
  fitpar<-names(sim_all[1,])[substr(names(sim_all), 1, 3)=="fit"]
  
  # parameters of interest
  PoI<-fitpar[!is.na(sim_all[1,fitpar])]
  
  # create a list for containing it
  list_plot<-list()
  
  for (p in 1:length(PoI)){

    parname<-substr(PoI[p], 4, nchar(PoI[p]))

    simpar<- paste0("sim", parname)

    fitpar<-PoI[p]

    currplot<- ggplot(sim_all, aes_string(
      x = paste0("sim", substr(PoI[p], 4, nchar(PoI)[p])) , y = PoI[p])) +
      geom_point()+
      geom_smooth(method=lm)+
      theme_classic()+
      stat_cor(method="pearson")+
      ggtitle( paste(parname, "parameter")
)
      # assign(paste0("plot", p), (currplot))
    #Sys.sleep(5)
    list_plot[[paste0("plot", parname)]]<-currplot

  }

# print
if (!file.exists(paste0(abs_path, "/figures/parameter_recovery_", model_name, ".pdf"))){

pdf(paste0(abs_path, "/figures/parameter_recovery_", model_name, ".pdf"))

for (l in 1: length(list_plot)){
print(list_plot[[l]])
}
dev.off ()

}
}
