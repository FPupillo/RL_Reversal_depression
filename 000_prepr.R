#------------------------------------------------------------------------------#
# script that renames the IDs of the duplicate Ids file.
# please read the "changelog.md" file for the detaliles. 
# create by francesco pupillo, "Wed Oct  9 09:23:39 2024" 
#------------------------------------------------------------------------------#

# remove all objects in the environment
rm(list=ls())

# get all the files in the raw data
files<-list.files("raw_data", pattern = ".csv")

# copy all the files in the "data_RL_screen" folder
if(!dir.exists("data_RL_screen")){
  dir.create("data_RL_screen")
}

# delete all the files in that directory if there are any
file.remove(list.files("data_RL_screen", full.names = T))


# copy all the raw files in that folder
for (f in files){
  file.copy(from = paste0("raw_data/", files), 
            to = paste0("data_RL_screen"))
  
}

# get the IDs
IDs<-vector()

for (n in 1:length(files)){

IDs[n]<-sub("_.*", "", files[n])

}

# get the duplicates
duplicates<-IDs[duplicated(IDs)]

# files in the new foler
files_RL_screen<-list.files("data_RL_screen")

# loop through the files
for (dup in duplicates){
  
  # get the names of the files with that (duplicated) ID
  c_dup<-grep(pattern =  paste0("^",dup, "."), files_RL_screen)
  
  # get the names of those files
  c_files<-files_RL_screen[c_dup]
  
  # get the second one
  c_file_name<-c_files[2]
  
  c_file<-read.csv(paste0("data_RL_screen/", c_file_name))
  
  # delete that file in the new "data_RL_screen" folder
  file.remove(paste0("data_RL_screen/",c_file_name))
  
  # rename the participant's ID within that file as the current
  # participants' ID +1
  c_file$participant<-c_file$participant[1]+1
  
  # now we also want to rename the file with that ID
  c_file_postname<-sub(paste0( dup, "*") , "", c_file_name)
  
  # create the new file name
  new_file_name<-paste0(c_file$participant[1], c_file_postname)
  
  # write the file
  write.csv(c_file, paste0("data_RL_screen/", new_file_name), 
            row.names = F)
  
  
  
}

