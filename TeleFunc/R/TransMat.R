transMat<-function(State_rec,prob=FALSE,States=NULL){
  if(is.null(States)){
    States<-unique(State_rec)
  }
  Stat_Num<-length(States)
  tot<-length(State_rec)
  
  state_rec<-State_rec[1:(tot-1)]
  nextState<-State_rec[2:(tot)]

  mat<-matrix(0,nrow=Stat_Num,ncol=Stat_Num)
  
  for(i in 1:Stat_Num){
    for(z in 1:Stat_Num){
      occur<-length(which(state_rec==States[z] & nextState==States[i]))
      mat[i,z] <- mat[i,z]+occur
    }
  }
  colnames(mat)<-States
  rownames(mat)<-States
  if(prob==TRUE){
    return(countProb(mat))
  }else  return(mat)
}


countProb<-function(transMat){
  count<-colSums(transMat)
  for(i in 1:ncol(transMat)){
    if(count[i]==0){
      transMat[,i]<-0
    }else{
      transMat[,i]<-transMat[,i]/count[i]
    }
  }
  return(transMat)
}






