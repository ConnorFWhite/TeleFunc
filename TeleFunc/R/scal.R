scal<-function(x,scal=c(0,1),lims=NULL,sqrt=F,na.rm=F,limit=TRUE){
  if(sqrt==T){
    x<-sqrt(x)
  }
  if(!is.null(lims)){
    x.min<-lims[1]
    x.max<-lims[2]
  }else{
    x.max<-max(x,na.rm=na.rm)
    x.min<-min(x,na.rm=na.rm)
  }
  
  if(x.max != x.min){
    xrange<-x.max-x.min
    scaled<-(x-x.min)/xrange*(scal[2]-scal[1])+scal[1]
    if(limit==TRUE){
      scaled[scaled>scal[2]]<-scal[2]
      scaled[scaled<scal[1]]<-scal[1]
    }
    return(scaled)
  }else{
    x[]<-scal[2]
    return(x)
  }
}

