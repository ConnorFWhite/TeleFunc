
angFold <- function(x,thresh = pi, na.rm=TRUE){
  if(any(x>thresh,na.rm)){
    loc<-which(x > thresh)
    x[loc]<-x[loc]-2*thresh
  }
  if(any(x < -thresh,na.rm)){
    loc<-which(x < -thresh)
    x[loc]<-x[loc] + 2*thresh
  }
  return(x)
}