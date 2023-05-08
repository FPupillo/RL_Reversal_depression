#------------------------------------------------------------------------------#
# extract memory data
#
# written by Francesco Pupillo
#
# [1] "Mon May  8 17:45:40 2023"
# 
#------------------------------------------------------------------------------#
rm(list=ls())

# source file with the functions to get memory data
source("helper_functions/sign_det.R")

# get the directory were the data are
data_path<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk/data/"

files<-list.files(data_path, pattern = ".csv")

# create an empty object
all_data_mem<-list()

# counter for the iterations
counter<-1

# loop through the files
for (f in files){
  
  # catch any errors that there migth be
  tryCatch({
  
  # get the data
  c_df<-read.csv(paste0(data_path,"/", f))
  
  # subset only the memory data
  c_df_mem<-c_df[!is.na(c_df$Memory_cond),]
  
  # create old/new response
  c_df_mem$rec_resp<-ifelse(c_df_mem$key_resp.keys>3, "old", "new")
  
  # get the sub num
  c_sub<-unique(c_df$participant)
  
  # get memory discrimination performance
  mem_disc<-sign_det(df = c_df_mem, sub_n = c_sub)
  
  # append to the dataset
  all_data_mem[[counter]]<-mem_disc  
  
  # update the coutner
  counter<-counter+1
  
  # return errors
  },
  error = function(e){paste0("problem with part ", c_sub, ": ", e)}
  )
  
}

# unpack the list
all_df<-do.call(rbind, all_data_mem)

# delet the rownames
rownames(all_df)<-NULL
