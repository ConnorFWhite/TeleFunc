locMap<-function(state_rec,Lats,Longs){
  Stations<-unique(state_rec)
  locs<-data.frame(matrix(nrow=length(Stations),ncol=3))
  colnames(locs)<-c("Station","Lats","Longs")
  for(i in 1:length(Stations)){
    Lat<-Lats[(which(state_rec==Stations[i])[1])]
    Long<-Longs[(which(state_rec==Stations[i])[1])]
    locs[i,]<-c(as.character(Stations[i]),Lat,Long)
  }
  locs[,2]<-as.numeric(locs[,2])
  locs[,3]<-as.numeric(locs[,3])
  return(locs)
}

logGen<-function(state_rec,times,Time_Step,start,end,states=NULL){
  require(plyr)
  if(is.null(states)){
    states<-unique(state_rec)
  }
  Date_P<-as.numeric(times)
  Start_P<-start
  End_P<-end
  start<-as.numeric(start)
  end<-as.numeric(end)
  end<-end-start
  Rel_Time<-Date_P-start
  Rel_Time<-round_any(x = Rel_Time,accuracy = Time_Step)
  
  T_Step<-seq(0,end,by=Time_Step)
  
  #Creating log of detection
  log<-matrix(0,nrow=length(T_Step),ncol=length(states))
  for(i in 1:length(T_Step)){
    l<-which(Rel_Time==T_Step[i])
    if(length(l)>0){
      for(z in 1:length(l)){
        log[i,which(states==state_rec[l[z]])]<-log[i,which(states==state_rec[l[z]])]+1
      }
    }
  }
  rownames(log)<-seq(Start_P,End_P,by=Time_Step)
  colnames(log)<-states
  return(log)
}

meanPos<-function(log,recLoc){
  loc<-which(rowSums(log)>0)
  r<-sapply(loc, FUN = function(x){
    count<-log[x,]
    y<-sum(recLoc[,2]*count)/sum(count)
    x<-sum(recLoc[,3]*count)/sum(count)
    return(c(y,x))
  })
  r<-t(r)
  return(r)
}




