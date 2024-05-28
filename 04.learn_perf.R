#------------------------------------------------------------------------------#
# learning performance
#------------------------------------------------------------------------------#

rm(list=ls())

library(dplyr)
library(ggplot2)
library(Rmisc)
library(lme4)
library(car)

# we are using the data that Stefanie said they have RL scoring above chance
path_to_data<-"~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/data_RL_screen_scoring"

# get the data 
data_files<-list.files(path_to_data, pattern = ".csv")

#questionnaires files
quest<-read.csv(paste0(dirname(path_to_data), "/", "Output_scales_39.csv"))

# now loop through the files and get the learning from learning and punish
all_data_encoding<-vector()

for (n in 1:length(data_files)){
  
  c_file<-read.csv(paste0(path_to_data, "/", data_files[n]))
  
  rel_from_pos<-mean(c_file$key_resp_trial.corr[c_file$Block_cond == "reversal_N_P"])
  
  rel_from_neg<-mean(c_file$key_resp_trial.corr[c_file$Block_cond == "reversal_P_N"])
  
  learning_asym<-rel_from_pos-rel_from_neg
  
  c_wide<-data.frame(c_file$ID[1], rel_from_pos, rel_from_neg, learning_asym, 
                              c_file$BDI_score[1], c_file$SHAPS_score[1])
  
  names(c_wide)[c(1, 5, 6)]<-c("ID", "BDI", "SHAPS")
  
  all_data_encoding<-rbind(all_data_encoding, c_wide)
  
}

# asymmetry index
all_data_encoding$asymmetry_index<-(all_data_encoding$rel_from_pos-all_data_encoding$rel_from_neg)/
  (all_data_encoding$rel_from_pos+all_data_encoding$rel_from_neg)

# is there a correlation between learning asymmetries and BDI?
cor.test(all_data_encoding$asymmetry_index, all_data_encoding$BDI)

cor.test(all_data_encoding$asymmetry_index, all_data_encoding$SHAPS)

cor.test(all_data_encoding$learning_asym, all_data_encoding$SHAPS)

plot(all_data_encoding$learning_asym, all_data_encoding$SHAPS)

# write
write.csv(all_data_encoding, "group_data/learning_asymmetries.csv")

#------------------------------------------------------------------------------#
custom_param<- function(){ theme(
  plot.title = element_text(size = 60),
  axis.title.x = element_text(size = 38),
  axis.title.y = element_text(size = 35),
  axis.text=element_text(size=28),
  legend.text=element_text(size=rel(5)),
  legend.title = element_text(size=rel(5)), 
  strip.text.x = element_text(size=50)
)}

# Learning asymmetry and questionnaires
ggplot(all_data_encoding, 
       aes( x=BDI, y=learning_asym))+
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  #geom_line(stat="smooth",method = "lm", formula=y~x, alpha=0.5, se=F)+
  geom_point(size = 5, alpha = 0.5, colour = 'red')+
  
  # specify that we want different colours for different participants
  #aes(colour = factor(participant))+
  # add the summary line with geom_smooth
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  theme(legend.position = "none")+
  ylab("Positive - Negative Block Accuracy ")+
  custom_param()

ggplot(all_data_encoding, 
      aes( x=SHAPS, y=learning_asym))+
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  #geom_line(stat="smooth",method = "lm", formula=y~x, alpha=0.5, se=F)+
  geom_point(size = 5, alpha = 0.5, colour = 'blue')+
  
  # specify that we want different colours for different participants
  #aes(colour = factor(participant))+
  # add the summary line with geom_smooth
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  theme(legend.position = "none")+
  ylab("Positive - Negative Block Accuracy ")+
  custom_param()
  
