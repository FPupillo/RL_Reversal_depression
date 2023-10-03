#------------------------------------------------------------------------------#
# script to check that everything is good with the continencies in 
# the different files
# created "Thu Sep 21 11:10:52 2023"
#------------------------------------------------------------------------------#

rm(list=ls())

library(dplyr)
library(ggplot2)
path_to_data<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk-master 2/data/"

# get the data 
data_files<-list.files(path_to_data, pattern = ".csv")

# select only the last participants
part_to_inspect<-as.character(c(17, 20, 24, 28, 30))

# select only those
data_files<-data_files[substr(data_files, 1, 2) %in% part_to_inspect]

# theere are two 28. We need to select only the second one. 
data_files<-data_files[data_files!="28_Experiment_Tuerk_2023-07-10_16h57.45.600.csv"]

# they were all collected the 18th of novemember
# import all in one file
all_data_practice<-vector()
all_data_encoding<-vector()
all_data_retrieval<-vector()
for (n in 1:length(data_files)){
 
# read the file 
 c_file<-read.csv(paste0(path_to_data, data_files[n]))
 
 # select the practice files
 c_file_practice<-c_file[c_file$Block_nr=="Practice",]

 # select VoI
 VoI_pract<-c("participant", "Block_nr","Block_cond", "Symbol_left","Symbol_right" ,
              "Highlighted" ,"Outcome", "corrAns", "Image" ,"Trial_within_block", "Trial_cond",
              "key_ass","answer_left", "answer_right" ,"key_resp_trial.keys" ,"key_resp_trial.corr" )
 
 
 c_file_practice<-c_file_practice[, VoI_pract]
 
 # get cumulative accuracy
 c_file_practice$cum_acc<-cummean(c_file_practice$key_resp_trial.corr)
 
 # create the trial number
 c_file_practice$trial_num<-1:nrow(c_file_practice)
 
 all_data_practice<-rbind(all_data_practice, c_file_practice)
 
 # now encoding
 c_file_encoding<-c_file[c_file$Block_nr %in% c(1:4),]
 
 VoI_encoding<-VoI_pract
 
 c_file_encoding<-c_file_encoding[, VoI_encoding]
 
 
 all_data_encoding<-rbind(all_data_encoding, c_file_encoding)
 
 # now retrieval files
 c_file_retrieval<-c_file[nchar(c_file$Phase)>1,]
 
 VoI_retrieval<-c('participant', "key_resp_memory.keys" ,"key_resp_memory.rt" ,
                  "key_resp_memory_trials.keys", "key_resp_memory_trials.rt" )
 c_file_retrieval<-c_file_retrieval[, VoI_retrieval]
 
 
 all_data_retrieval<-rbind(all_data_retrieval, c_file_retrieval)
  
}

# create symbol highlighted
for ( file in c("practice", "encoding")){
  
  df<-get(paste0("all_data_", file))
  
  df$symbol_highlighted<-ifelse(df$Highlighted == "left", df$Symbol_left, 
                                df$Symbol_right)
  
  assign(paste0("all_data_", file), df)
  
}

# plot cumAcc
all_data_practice$Trial_within_block<-as.numeric(all_data_practice$trial_num)
ggplot(all_data_practice, aes(x = trial_num, y = cum_acc , group=1)) +
  stat_summary(fun.y="mean",geom="line", size = 1.5)+
facet_grid(symbol_highlighted~participant)  +
  theme_classic()

# calculate accuracy
accuracy_pract<-all_data_encoding %>%
  group_by(participant) %>%
  summarise(accuracy = mean(key_resp_trial.corr, na.rm = T))

#now filtering for congruency
accuracy_pract_1<-all_data_practice%>%
  filter(Trial_cond ==1)  %>%
  group_by(participant) %>%
  summarise(accuracy = mean(key_resp_trial.corr, na.rm = T))

# now calcualate the percentage of each choice for practice
all_data_practice%>%
  group_by(participant, symbol_highlighted, Outcome) %>%
   summarise(n = n()) %>%
   mutate(freq = n / sum(n))

# now calcualate the percentage of each choice for LEARNING
enc<-all_data_encoding%>%
  group_by(participant, symbol_highlighted, Outcome, Block_cond) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#
  