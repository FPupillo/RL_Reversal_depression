sign_det<-function(df, sub_n){
  #----------------------------------------------------------------------------#
  # create signal detection values 
  #----------------------------------------------------------------------------#
  #
  # calculate dprime
  library(psycho)
 
  # compute accuracy
 df$recog_acc<-NA
  for (n in 1:nrow(df)){
    if (!is.na(df$Memory_cond[n])){
      if (df$Memory_cond[n] == "Old"){
        if (df$key_resp_memory_trials.keys[n]>=4){
          df$recog_acc[n]<- 1 }else{   df$recog_acc[n]<- 0}
      }else if(df$Memory_cond[n] == "New"){
        if (df$key_resp_memory_trials.keys[n]<4){
          df$recog_acc[n]<- 1 }else{   df$recog_acc[n]<- 0}
      }
    }
  }
  
 # recognition
 # calculate hit, miss, rej, and FA
 df$recog_type<-NA
 for ( i in 1:nrow(df)){
   if (df$Memory_cond[i]=="Old"& df$recog_acc[i]==1 ){
     df$recog_type[i]<-"HIT"
   } else if (df$Memory_cond[i]=="Old"& df$recog_acc[i]==0 ){
     df$recog_type[i]<-"Miss"
   } else if(df$Memory_cond[i]=="New"& df$recog_acc[i]==1  ){
     df$recog_type[i]<-"CorrRej"
   } else if (df$Memory_cond[i]=="New"& df$recog_acc[i]=="0" ){
     df$recog_type[i]<-"FA"
   }
 }
 
 VoI<-c("recog_type")
 
 # wide dataset
 wideData<-table(df[,VoI])
 
 # convert it to a data.frame
 wideData<-as.data.frame(rbind(wideData))

 # check if there are any null values
 vars<-c("CorrRej", "HIT", "Miss", "FA")
 for (var in vars){
   
   if(! any(names(wideData)==var)){ # if we do not have any occurrence, we create it
     
     wideData[[var]]<-0
     
   }
   
 }
 
 # compute percentage HIT
 wideData$HITrate<-wideData$HIT/(wideData$HIT+wideData$Miss)
 
 # percentage false alarm
 wideData$FArate<-wideData$FA/(wideData$FA+wideData$CorrRej)
 
 indices <- psycho::dprime(wideData$HIT, wideData$FA, wideData$Miss, wideData$CorrRej)
 wideData<-cbind(wideData, indices)
 
 # PR
 wideData$hitMinFA<-wideData$HITrate-wideData$FArate
 
 wideData$participant<-sub_n
 
 return(wideData)
 
}