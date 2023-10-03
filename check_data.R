#------------------------------------------------------------------------------#
# script to check that everything is good with the continencies in 
# the different files
# created "Thu Sep 21 11:10:52 2023"
#------------------------------------------------------------------------------#

rm(list=ls())

library(dplyr)
library(ggplot2)
library(Rmisc)
library(lme4)
library(car)

source("helper_functions/cum_acc_window.R")
#path_to_data<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk-master 2/data/"

# we are using the data that Stefanie said they have RL scoring above chance
path_to_data<-"~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/data_RL_screen"

# get the data 
data_files<-list.files(path_to_data, pattern = ".csv")

#questionnaires files
quest<-read.csv(paste0(dirname(path_to_data), "/", "Output_scales_39.csv"))

# select only the participants who Stefanie said they scored above chance 
#part_to_inspect<-as.character(c(17, 20, 24, 28, 30))

# select only those
#data_files<-data_files[substr(data_files, 1, 2) %in% part_to_inspect]

# theere are two 28. We need to select only the second one. 
#data_files<-data_files[data_files!="28_Experiment_Tuerk_2023-07-10_16h57.45.600.csv"]

#------------------------------------------------------------------------------#
# loop through data, assign the correspondent BDI and SHAPS, and assign a 
# progressive participant number

for (number in 1:length(data_files)){
  
  c_file_name<-data_files[number]
  
  c_file<-read.csv(paste0(path_to_data, "/",c_file_name))
  
  # take out "_Experiment_Tuerk_" from the file, to match the questionnaire file
  file_name<-gsub('_Experiment_Tuerk_',' ',c_file_name)
  
  # now the .csv
  file_name<-gsub('.csv','',file_name)
  
  # get the Shaps and BIDS
  c_quest<-quest[quest$participant == file_name,]
  
  # assign the BDI and the SHAPS
  c_file$BDI_score<-c_quest$BDI_score
  
  c_file$SHAPS_score<-c_quest$SHAPS_score
  
  c_file$PANAS_neg_score<-c_quest$PANAS_neg_score
  
  c_file$PANAS_pos_score<-c_quest$PANAS_pos_score
  
  # now the ID
  c_file$ID<-number
  
  # now save it
  write.csv(c_file, paste0(dirname(path_to_data), "/data_RL_screen_scoring/",
                   paste0(number, ".csv")), row.names = F)
  
    
}
#------------------------------------------------------------------------------#

path_to_data2<-paste0(dirname(path_to_data), "/data_RL_screen_scoring/")
data_files2<-list.files(path_to_data2)

# they were all collected the 18th of novemember
# import all in one file
all_data_practice<-vector()
all_data_encoding<-vector()
all_data_retrieval<-vector()
for (n in 1:length(data_files)){
 
# read the file 
 #c_file<-read.csv(paste0(path_to_data, data_files[n]))
  c_file<-read.csv(paste0(path_to_data2, data_files2[n]))
 # select the practice files
 c_file_practice<-c_file[c_file$Block_nr=="Practice",]

 # select VoI
 VoI_pract<-c("participant", 'ID', "Block_nr","Block_cond", "Symbol_left","Symbol_right" ,
              "Highlighted" ,"Outcome", "corrAns", "Image" ,"Trial_within_block", "Trial_cond",
              "key_ass","answer_left", "answer_right" ,"key_resp_trial.keys" ,"key_resp_trial.corr",
              "BDI_score", "SHAPS_score")
 
 
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
 
 # get cum_acc winsow
 c_file_encoding<-cum_acc_window(c_file_encoding, n=5, var_name = "key_resp_trial.corr")
 
 # create the trial number
 c_file_encoding$trial_num<-1:nrow(c_file_encoding)
 
 # create trial num by round
 # create round
 c_file_encoding$round<-ifelse(c_file_encoding$Block_nr<3, 1, 2)
 
 c_file_encoding$trial_num_round<-rep(1:64, times = 2)
 
 all_data_encoding<-rbind(all_data_encoding, c_file_encoding)
 
 
 # now retrieval files
 c_file_retrieval<-c_file[nchar(c_file$Phase)>1,]
 
 VoI_retrieval<-c('participant', 'ID', "key_resp_memory.keys" ,"key_resp_memory.rt" ,
                  "key_resp_memory_trials.keys", "key_resp_memory_trials.rt", "BDI_score", 
                  "SHAPS_score", VoI_pract)
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

# create the acquisition-reversal variable
all_data_encoding$block_type<-sub("\\_.*", "", all_data_encoding$Block_cond)
all_data_encoding$block_valence<-ifelse(
  all_data_encoding$Block_cond =="acquisition_P" |
   all_data_encoding$Block_cond =="reversal_P_N",
  "Pos_to_Neg", "Neg_to_Pos")

# valence
all_data_encoding$valence<-substr(all_data_encoding$Block_cond, 
                                  nchar(all_data_encoding$Block_cond), 
                                  nchar(all_data_encoding$Block_cond)  )

# save it
write.csv(all_data_encoding, "group_data/all_data_encoding.csv", row.names = F)


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

# plot
# sumamrise
dat_summary_fix_pr<- summarySEwithin(all_data_encoding,
                                     measurevar = "cum_acc_window",
                                     withinvars = c("trial_num_round",  "block_valence") , 
                                     idvar = "ID", 
                                     na.rm = T)

custom_param<- function(){ theme(
  plot.title = element_text(size = 40),
  axis.title.x = element_text(size = 38),
  axis.title.y = element_text(size = 28),
  axis.text=element_text(size=28),
  legend.text=element_text(size=rel(2)),
  legend.title = element_text(size=rel(2)), 
  strip.text.x = element_text(size=28)
)}

dat_summary_fix_pr$trial_num_round<-as.numeric(dat_summary_fix_pr$trial_num_round)
ggplot(dat_summary_fix_pr[dat_summary_fix_pr$trial_num_round<59,],
                    aes(x = trial_num_round, y = cum_acc_window, 
                              # this assigns the color to the group
                              color = block_valence, fill = block_valence, 
                              # this changes the line type as a funciton of group
                              #linetype = block_type, 
                              group = 1))+
stat_summary(fun.y="mean",geom="line", size = 1.5)+
  
# this add the shadow considering the within-participant standard error
  geom_ribbon(aes(ymin=cum_acc_window-se, ymax=cum_acc_window+se), alpha=0.5, colour=NA)+
  # divide the plot as a funtion of age group
  facet_wrap(.~block_valence)+
  # customise the breaks
  #scale_x_continuous(breaks=seq(1, 100, 19))+
  
  # add personalized colours
  #scale_color_manual(values = c(c( "#AA4499" ,"#44AA99")))+
  
  # add personalized parmaeters
  custom_param()+
  
  theme(plot.title = element_text(hjust = 0.5))+
  # use the classic theme
  theme_classic()


#all_data_encoding$Trial_within_block<-as.numeric(all_data_practice$trial_num)
ggplot(all_data_encoding, aes(x = trial_num_round, y = cum_acc_window , group=1))+
  stat_summary(fun.y="mean",geom="line", size = 1.5)+
  facet_wrap(block_type~.)  +
  geom_vline(xintercept = c(32, 64, 96 ))+
  theme_classic()



# now bars
dat_summary_pr_2<- summarySEwithin(all_data_encoding,
                                     measurevar = "key_resp_trial.corr",
                                     withinvars = c("block_type",  "valence") , 
                                     idvar = "ID", 
                                     na.rm = T)

ggplot(all_data_encoding %>%
         group_by(ID, block_type, valence)%>%
         dplyr::summarise(key_resp_trial.corr=mean(key_resp_trial.corr , na.rm=T)), 
       aes(x = valence, y = key_resp_trial.corr, color = valence ))+
  geom_point(alpha = 0.10, colour = "black" )+

  # add a line that connect those points by participant
  geom_line( aes(valence, key_resp_trial.corr ,group = ID),
             size=1, alpha=0.1, stat="summary" , colour = 'black')+
  # add a summary line (mean)
  geom_point(stat="summary", size = 5, data = dat_summary_pr_2)+
  xlab("")+
  # add the within-participant confidence intervals
  geom_errorbar(aes( y = key_resp_trial.corr, ymin = key_resp_trial.corr - ci, 
                     ymax = key_resp_trial.corr + ci),
                width = 0.40,size = 1.5, data=dat_summary_pr_2)+
  # divide the plot as a function of age group
  facet_wrap(.~block_type)+
  theme_classic()+
  custom_param()+
  # delete the x axis text and ticks, as we have the legend
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ylab("Accuracy")

# analyze
mod1<-glmer(key_resp_trial.corr~valence*block_type +(valence*block_type|ID), family = binomial, 
            data = all_data_encoding, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

summary(mod1)
Anova(mod1)

# does BDI modulates?
mod1BDI<-glmer(key_resp_trial.corr~valence*block_type*BDI_score +(valence*block_type|ID), family = binomial, 
               data = all_data_encoding, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

Anova(mod1BDI)

mod1SHAPS<-glmer(key_resp_trial.corr~valence*block_type*SHAPS_score +(valence*block_type|ID), family = binomial, 
               data = all_data_encoding, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )
Anova(mod1SHAPS)

# only on reversals
mod1SHAPS_rev<-glmer(key_resp_trial.corr~valence*SHAPS_score +(valence|ID), family = binomial, 
                 data = all_data_encoding[all_data_encoding$block_type == "reversal",], 
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )
Anova(mod1SHAPS_rev)
summary(mod1SHAPS_rev)
# without valence
mod1SHAPS_revnoval<-glmer(key_resp_trial.corr~SHAPS_score +(1|ID), family = binomial, 
                     data = all_data_encoding[all_data_encoding$block_type == "reversal",], 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )
summary(mod1SHAPS_revnoval)

# the higher the SHAPS< the better they perform in general after a reversal

# now BDI
mod1BDI_rev<-glmer(key_resp_trial.corr~valence*BDI_score +(valence|ID), family = binomial, 
                     data = all_data_encoding[all_data_encoding$block_type == "reversal",], 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

Anova(mod1BDI_rev)

# extract the beta values for valence and correlate them with the BDI
