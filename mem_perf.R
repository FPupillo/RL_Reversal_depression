#------------------------------------------------------------------------------#
# extract memory data
#
# written by Francesco Pupillo
#
# [1] "Mon May  8 17:45:40 2023"
# 
#------------------------------------------------------------------------------#
rm(list=ls())

library(viridis)
library(ggplot2)
library(lmerTest)
library(lme4)
# source file with the functions to get memory data
source("helper_functions/sign_det.R")
source("helper_functions/dprime_thres.R")


# get the directory were the data are
#data_path<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk/data/"
data_path<-paste0("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/data_RL_screen_scoring/")

group_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/group_data/all_data_encoding.csv")
asym_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/group_data/learning_asymmetries.csv")

                     
files<-list.files(data_path, pattern = ".csv")

# create an empty object
all_data_mem<-list()
all_data_long<-vector()

# counter for the iterations
counter<-1

# loop through the files
for (f in files){
  
  # catch any errors that there migth be
  tryCatch({
  
  print(f)
  # get the data
  c_df<-read.csv(paste0(data_path,"/", f))
  
  # subset only the memory data
  c_df_mem<-c_df[nchar(c_df$Memory_cond)>0,]
  
  # create old/new response
  c_df_mem$rec_resp<-ifelse(c_df_mem$key_resp_memory_trials.keys>3, "old", "new")
  
  # get the sub num
  c_sub<-unique(c_df$ID)
  
  print(c_sub)
  
  # get memory discrimination performance
  mem_disc<-sign_det(df = c_df_mem, sub_n = c_sub)
  
  # append to the dataset
  all_data_mem[[counter]]<-mem_disc  
  
  # update the coutner
  counter<-counter+1

  # now link the variables in encoding to the retrieval phase
  # select only old
  c_df_mem<- c_df_mem[c_df_mem$Memory_cond=="Old",]
  
  # get variables from group data
  c_learn_data<-group_data[group_data$ID %in% c_sub,]
  
  # get asymmetry data
  c_asym<-asym_data[asym_data$ID == c_sub,]
  
  # get only the 
  
  for (n in 1:nrow(c_df_mem)){
    
    c_df_mem$Block_cond[n]<-c_learn_data$Block_cond[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$prediction_accuracy[n]<-c_learn_data$key_resp_trial.corr[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$Outcome[n]<-c_learn_data$Outcome[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$round[n]<-c_learn_data$round[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$trial_num_round[n]<-c_learn_data$trial_num_round[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$block_valence[n]<-c_learn_data$block_valence[c_learn_data$Image == c_df_mem$Image[n]]
    c_df_mem$valence[n]<-c_learn_data$valence[c_learn_data$Image == c_df_mem$Image[n]]

  }
  
  # now the asym
  VoI<-c("rel_from_pos","rel_from_neg" , "learning_asym" ,
         "asymmetry_index")
  
  for (var in VoI){
    
    c_df_mem[[var]]<- c_asym[[var]]
    
  }
  
  var_sel<-c("ID", VoI, "Block_cond","prediction_accuracy", "prediction_accuracy" ,"round", "trial_num_round",
             "block_valence" , "valence" ,"key_resp_memory_trials.keys" ,
             "BDI_score", "SHAPS_score", "Outcome")
  
  c_df_mem<-c_df_mem[, var_sel]
  
  all_data_long<-rbind(all_data_long, c_df_mem)
  
  print(c_df_mem$ID[1])
  
  # return errors
  },
  error = function(e){paste0("problem with part ", c_sub, ": ", e)}
  )
  
}

# unpack the list
all_df<-do.call(rbind, all_data_mem)

# delet the rownames
rownames(all_df)<-NULL

#------------------------------------------------------------------------------#
# get the cutoff for dprime
# create the threshold for excluding participants through a permutation analysis
permutations <-dprime_thres(128, 128, 5000, 0.95)

thres<-permutations$thres
null_distr<-permutations$distribution

# create a all dataframe with the two distributions
# first the simulated
null_distr<-as.data.frame(null_distr)
null_distr$participant<-1:nrow(null_distr)
null_distr$participant<-null_distr$participant+200 # to distinguish with the actual partic
names(null_distr)[1]<-"dprime"

null_distr$type<-"Null"

# now the actual
emp<-all_df[, c("dprime", "participant")]

emp$type<-"Empirical"

# merge them
distr_all<-rbind(null_distr, emp)

# plot 
PlotBind<-ggplot(distr_all, aes(x= dprime, fill=type))

PlotBind+
  #geom_histogram(aes(y = ..density..),
  #            colour = c(1,2), fill = "white") +
  geom_density(alpha = .5)+
  theme_classic()+
  geom_boxplot()+
  geom_vline(xintercept = thres)+
  #custom_param()+
  xlab("d'")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(discrete=TRUE, option = "viridis") 

# exclude those that fall below the threshold
part_exc<-all_df$participant[all_df$dprime<thres]

all_data_long_excl<-all_data_long[!all_data_long$ID %in% part_exc,]


# 9 participants fall below the threshold
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# analyze memory hits
# First, as a function of block
custom_param<- function(){ theme(
  plot.title = element_text(size = 40),
  axis.title.x = element_text(size = 38),
  axis.title.y = element_text(size = 28),
  axis.text=element_text(size=28),
  legend.text=element_text(size=rel(2)),
  legend.title = element_text(size=rel(2)), 
  strip.text.x = element_text(size=28)
)}

# get block type
all_data_long$block_type<-sub("\\_.*", "", all_data_long$Block_cond)


# now bars
dat_summary<- summarySEwithin(all_data_long,
                                   measurevar = "key_resp_memory_trials.keys",
                                   withinvars = c("block_type",  "valence") , 
                                   idvar = "ID", 
                                   na.rm = T)

ggplot(all_data_long %>%
         group_by(ID, block_type, valence)%>%
         dplyr::summarise(key_resp_memory_trials.keys=mean(key_resp_memory_trials.keys, na.rm=T)), 
       aes(x = valence, y = key_resp_memory_trials.keys, color = valence ))+
  geom_point(alpha = 0.10, colour = "black" )+
  
  # add a line that connect those points by participant
  geom_line( aes(valence, key_resp_memory_trials.keys ,group = ID),
             size=1, alpha=0.1, stat="summary" , colour = 'black')+
  # add a summary line (mean)
  geom_point(stat="summary", size = 5, data = dat_summary)+
  xlab("")+
  # add the within-participant confidence intervals
  geom_errorbar(aes( y = key_resp_memory_trials.keys, ymin = key_resp_memory_trials.keys - ci, 
                     ymax = key_resp_memory_trials.keys + ci),
                width = 0.40,size = 1.5, data=dat_summary)+
  # divide the plot as a function of age group
  facet_wrap(.~block_type)+
  theme_classic()+
  custom_param()+
  # delete the x axis text and ticks, as we have the legend
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ylab("Conf-weighted Accuracy")


# analyze
mod1<-lmer(key_resp_memory_trials.keys~block_type*valence +(block_type*valence|ID), 
           data = all_data_long,
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

Anova(mod1)

# only reversal
mod1_rev_BDI<-lmer(key_resp_memory_trials.keys~valence*BDI_score+(valence|ID), 
           data = all_data_long_excl[all_data_long_excl$block_type=="reversal",],
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_BDI)
summary(mod1_rev_BDI)

mod1_rev_SHAPS<-lmer(key_resp_memory_trials.keys~valence*SHAPS_score+(valence|ID), 
                   data = all_data_long_excl[all_data_long_excl$block_type=="reversal",],
                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_SHAPS)

# now add learning
mod1_rev_learn_asym<-lmer(key_resp_memory_trials.keys~learning_asym*valence+(valence|ID), 
                          data = all_data_long_excl[all_data_long_excl$block_type=="reversal",],
                          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_learn_asym)

summary(mod1_rev_learn_asym)

# from_pos
mod1_rev_learn_pos<-lmer(key_resp_memory_trials.keys~rel_from_pos*valence+(valence|ID), 
                          data = all_data_long[all_data_long$block_type=="reversal",],
                          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_learn_pos)
summary(mod1_rev_learn_pos)
# from_neg
mod1_rev_learn_neg<-lmer(key_resp_memory_trials.keys~rel_from_neg*valence+(valence|ID), 
                         data = all_data_long[all_data_long$block_type=="reversal",],
                         control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_learn_neg)

# outcome
mod1_rev_learn_neg<-lmer(key_resp_memory_trials.keys~rel_from_neg*valence+(valence|ID), 
                         data = all_data_long[all_data_long$block_type=="reversal",],
                         control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

mod1_rev_learn_neg<-lmer(key_resp_memory_trials.keys~prediction_accuracy*Outcome+(prediction_accuracy*Outcome|ID), data = all_data_long,
                         control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)) )

anova(mod1_rev_learn_neg)

