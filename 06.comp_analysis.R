#------------------------------------------------------------------------------#
# learning asymmetries and computational models
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(lme4)
library(reshape2)
library(Hmisc)

group_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/group_data/all_data_encoding.csv")
asym_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/group_data/learning_asymmetries.csv")
memory_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/group_data/memory.csv")
# computational data
computational_data<-read.csv("~/PowerFolders/Frankfurt_University/rev_aned_steffi_thesis/output_folder/parameter_estimation_RW_pav_alpha_gainloss_rho_gainloss.csv")

# get learning rate asymmetry
computational_data$gain_loss_asym<-computational_data$alphagain-
                                    computational_data$alphaloss

computational_data$rhogain_loss_asym<-computational_data$rhogain-
                                      computational_data$rholoss

#now, link the other parameters to the computational parameters
memory_data_wide<-dcast(memory_data,  ID+excl   ~ valence+block_type, 
                        value.var = "key_resp_memory_trials.keys")


#------------------------------------------------------------------------------#
VoI<-c(names(asym_data)[3:8])

for (var in (VoI)){
  
  for (n in 1:nrow(memory_data)){
    
    memory_data[[var]][n]<-asym_data[[var]][asym_data$ID == memory_data$ID[n]]
                                             
  }
}

# now the computational data
comp_VoI<-names(computational_data)[c(5:6, 8:9, 12:13)]

for (var in comp_VoI){
  
  for (n in 1: nrow(memory_data)){
    
    memory_data[[var]][n]<-computational_data[[var]][computational_data$PartNum == 
                                                       memory_data$ID[n]]
    
    
  }
  
}

#------------------------------------------------------------------------------#
# check if BDI and SHAPS affect memory

mod_BDI<-lmer(key_resp_memory_trials.keys~BDI*valence*block_type +(1|ID), 
              data = memory_data[memory_data$excl==0,])

anova(mod_BDI, type = 3)
summary((mod_BDI))

# only on reversal
mod_BDI_rev<-lmer(key_resp_memory_trials.keys~BDI*valence* +(1|ID), 
              data = memory_data[memory_data$excl==0 & memory_data$block_type=='reversal',])

anova(mod_BDI_rev)

summary(mod_BDI_rev)

# now SHAPS
mod_SHAPS<-lmer(key_resp_memory_trials.keys~SHAPS*valence*block_type +(1|ID), 
              data = memory_data)

anova(mod_SHAPS, type = 3)
summary((mod_SHAPS))

# only on reversal
mod_SHAPS_rev<-lmer(key_resp_memory_trials.keys~SHAPS*valence* +(1|ID), 
                  data = memory_data[memory_data$excl==0 & 
                                       memory_data$block_type=='reversal',])

anova(mod_SHAPS_rev)

summary(mod_SHAPS_rev)

#------------------------------------------------------------------------------#
#now learning asymmetries

mod_learn<-lmer(key_resp_memory_trials.keys~learning_asym*valence*block_type +(1|ID), 
                    data = memory_data[memory_data$excl==0 ,])

anova(mod_learn)
summary(mod_learn)

mod_learn_rev<-lmer(key_resp_memory_trials.keys~learning_asym*valence +(1|ID), 
                data = memory_data[memory_data$excl==0 & 
                                     memory_data$block_type=='reversal',])
anova(mod_learn_rev)
summary(mod_learn_rev)

#------------------------------------------------------------------------------#
# now computational parameters
# are they correlated with BDI and SHAPS?
cor_BDI_learn<-lmer(BDI~gain_loss_asym+(1|ID), data = memory_data )
anova(cor_BDI_learn)

cor_SHAPS_learn<-lmer(SHAPS~gain_loss_asym+(1|ID), data = memory_data )
anova(cor_SHAPS_learn)

cor_BDI_learn<-lmer(BDI~gain_loss_asym+(1|ID), data = memory_data )
anova(cor_BDI_learn)

cor_SHAPS_learn<-lmer(SHAPS~gain_loss_asym+(1|ID), data = memory_data )
anova(cor_SHAPS_learn)

cor_BDI_learn<-lmer(BDI~rhogain_loss_asym+(1|ID), data = memory_data )
anova(cor_BDI_learn)

cor_SHAPS_learn<-lmer(SHAPS~rhogain_loss_asym+(1|ID), data = memory_data )
anova(cor_SHAPS_learn)


anova(mod_com_alpha_BDI)


#------------------------------------------------------------------------------#
# now the memory data wide
VoI<-c(names(asym_data)[3:8])

for (var in (VoI)){
  
  for (n in 1:nrow(memory_data_wide)){
    
    memory_data_wide[[var]][n]<-asym_data[[var]][asym_data$ID == memory_data_wide$ID[n]]
    
    
    
  }
}

# now the computational data
comp_VoI<-names(computational_data)[c(5:6, 8:9, 12:13)]

for (var in comp_VoI){
  
  for (n in 1: nrow(memory_data_wide)){
    
    memory_data_wide[[var]][n]<-computational_data[[var]][computational_data$PartNum == 
                                                       memory_data_wide$ID[n]]
    
    
  }
  
}

#------------------------------------------------------------------------------
memory_data_wide$rev_PminusN<-memory_data_wide$P_reversal-memory_data_wide$N_reversal


# BDI on memory for pos and negative
custom_param<- function(){ theme(
  plot.title = element_text(size = 60),
  axis.title.x = element_text(size = 38),
  axis.title.y = element_text(size = 38),
  axis.text=element_text(size=28),
  legend.text=element_text(size=rel(2)),
  legend.title = element_text(size=rel(3)), 
  strip.text.x = element_text(size=50)
)}


ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = BDI, y = rev_PminusN))+
  geom_point(size = 5, alpha = 0.5, colour = 'red')+
  
geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+

theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Memory Pos Rev - Neg Rev")+
  xlab("BDI")

# analyze
model_wide_BDI<-lm(rev_PminusN~BDI, 
               data = memory_data_wide[memory_data_wide$excl==0,])

summary(model_wide_BDI)

# now SHAPS
ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = SHAPS, y = rev_PminusN))+
  geom_point(size = 5, alpha = 0.5, colour = 'blue')+
  
geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Memory Pos Rev - Neg Rev")+
  xlab("SHAPS")

### BDi and learning
ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = BDI, y = rhogain_loss_asym))+
  geom_point(size = 5, alpha = 0.5, colour = 'red')+
  
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Positive - Negative Sensitivity")+
  xlab("BDI")

ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = SHAPS, y = rhogain_loss_asym))+
  geom_point(size = 5 ,alpha = 0.5, colour = 'blue')+
  
  geom_smooth(method="lm",formula=y~x, se=T, colour = "black" )+
  
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Positive - Negative Sensitivity")+
  xlab("SHAPS")


# analyze
model_wide_BDI<-lm(rev_PminusN~BDI, 
                   data = memory_data_wide[memory_data_wide$excl==0,])

summary(model_wide_BDI)

library(corrplot)
mydata.cor = cor(memory_data_wide, method = c("spearman"))
corrplot(mydata.cor)

cor.test(memory_data_wide$rev_PminusN, 
         momemoy_data_wide)

cor.test(memory_data_wide$rel_from_pos, memory_data_wide$SHAPS)

cor.test(memory_data_wide$rhogain_loss_asym, memory_data_wide$N_reversal)

plot(memory_data_wide$rhogain_loss_asym, memory_data_wide$N_reversal)

cor.test(memory_data_wide$rhogain_loss_asym, memory_data_wide$P_reversal)

plot(memory_data_wide$rhogain_loss_asym, memory_data_wide$P_reversal)

cor.test(memory_data_wide$rhogain_loss_asym, memory_data_wide$rev_PminusN)

plot(memory_data_wide$rhogain_loss_asym, memory_data_wide$rev_PminusN)


model_wide<-lm(rev_PminusN~rhogain_loss_asym, 
                   data = memory_data_wide[memory_data_wide$excl==0,])

anova(model_wide)

summary(model_wide_BDI)

model_wide_BDI<-lm(rev_PminusN~rhogain_loss_asym*BDI, data = memory_data_wide)

anova(model_wide_BDI)

model_wide_SHAPS<-lm(rev_PminusN~rhogain_loss_asym*SHAPS, 
                     data = memory_data_wide[memory_data_wide$excl==0,])

anova(model_wide_SHAPS)

# learning rate
model_wide_BDI<-lm(rev_PminusN~gain_loss_asym*BDI, data = memory_data_wide)

anova(model_wide_BDI)

# now some plots
###################. first with BIDS
# dat_summary<- summarySEwithin(memory_data_wide[memory_data_wide$excl==0,],
#                               measurevar = "rev_PminusN",
#                               withinvars = c("block_type",  "valence") , 
#                               idvar = "ID", 
#                               na.rm = T)Ç

data2<-memory_data_wide


data2$BDI<-1
ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = rhogain_loss_asym, y = rev_PminusN, color = BDI))+
  geom_line(stat="smooth",method = "lm",fullrange=T, formula=y~x, alpha=0.7,
            size = 1.5, se=T)+
  aes(group = (BDI))+

  #geom_line(data = data2, stat="smooth",method = "lm", size = 1, se=T)+
  geom_smooth(data = data2, method=lm,se=T, size=3)+

  #geom_point(alpha = 0.10, colour = "black" )
  
  #geom_point( formula=y~x, alpha=0.5, se=F)+
  
  # specify that we want different colours for different participants
  #aes(colour = factor(participant))+
  # add the summary line with geom_smooth
  #geom_smooth(method=lm,se=FALSE,fullrange=TRUE,aes(color=BDI, alpha=0.3))
  #geom_line(datastat="smooth", method="lm",formula=y~x, se = F)
  #geom_smooth(method="lm",formula=y~x, se=F, colour = "black" )+
  
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Memory Pos Rev - Neg rev")+
  xlab("Pos - Neg Sensitivity")



# now SHAPS
data2$SHAPS<-1
memory_data_wide$SHAPS<-memory_data_wide$SHAPS+1
ggplot(memory_data_wide[memory_data_wide$excl==0,],
       aes(x = rhogain_loss_asym, y = rev_PminusN, color = SHAPS))+
  geom_line(stat="smooth",method = "lm",fullrange=T, formula=y~x, alpha=0.5,
            size = , se=F)+
  aes(group = SHAPS)+
  
  #geom_line(data = data2, stat="smooth",method = "lm", size = 1, se=T)+
  geom_smooth(data = data2, method=lm,se=T, size=3)+
  
  #geom_point(alpha = 0.10, colour = "black" )
  
  #geom_point( formula=y~x, alpha=0.5, se=F)+
  
  # specify that we want different colours for different participants
  #aes(colour = factor(participant))+
  # add the summary line with geom_smooth
  #geom_smooth(method=lm,se=FALSE,fullrange=TRUE,aes(color=BDI, alpha=0.3))
  #geom_line(datastat="smooth", method="lm",formula=y~x, se = F)
  #geom_smooth(method="lm",formula=y~x, se=F, colour = "black" )+

theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  #ggtitle("Experiment 2")+
  #theme(legend.position = "none")+
  custom_param()+
  ylab("Memory Pos Rev - Neg rev")+
  xlab("Pos - Neg Sensitivity")
