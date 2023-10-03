#------------------------------------------------------------------------------#
# select only the new files
# 
#------------------------------------------------------------------------------#
library(dplyr)
# clean the data 
rm(list=ls())

# get the script directory
path<-rstudioapi::getSourceEditorContext()$path

# split the string into the names of the folders
names<-unlist(strsplit(path, split="/"))

# get number of carachters last name (file name)
charfile<-nchar(tail(names,1))

# subtract that to the path
path<-substr(path, 1,nchar(path) - charfile)

# set wd 
setwd(path)

cd<-getwd()

# poath to raw data
raw_data_path<-"~/PowerFolders/Frankfurt_University/RL_reversal_depression/experiment_tuerk-master 2/data/"

#------------------------------------------------------------------------------#
# copy files to the preprocessed folder
if (! dir.exists("preproc_data")){
  
  dir.create("preproc_data")
  
}

setwd(paste0(path, "preproc_data"))

# remove all the files iht eh clean direactory
file.remove(list.files(paste0(path, "preproc_data"), include.dirs = T))

setwd(raw_data_path)

files<-list.files()

# we want only the .csv and the .edf
files_sel<-files[grep(".csv", files)]

# copy all the files from the raw to the preprocessed folder
file.copy( files_sel, paste0(path, "preproc_data"))

# set working dir in the preproc_folder
setwd(paste0(path, "preproc_data"))

# set the folder in the raw data
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# delete all the particiapnt files
file_to_del<-list.files(pattern = "^PARTICIPANT_.*")
file.remove(file_to_del)
#------------------------------------------------------------------------------#

