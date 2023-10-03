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

plot(all_data_encoding$learning_asym, all_data_encoding$SHAPS)

# write
write.csv(all_data_encoding, "group_data/learning_asymmetries.csv")
