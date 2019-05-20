circLines<-function(daty,datx,lims=c(-180,180),...){
  ys<-diff(daty)
  daty<-as.numeric(daty)
  datx<-as.numeric(datx)
  
  if(any(abs(ys)>((lims[2]-lims[1])/2))){
    
    loc<-which(abs(ys)>((lims[2]-lims[1])/2))
    datSeg<-data.frame(x0=datx[1:(length(datx)-1)],
                       x1=datx[2:length(datx)],
                       y0=daty[1:(length(datx)-1)],
                       y1=daty[2:length(datx)])
    segments(x0=datSeg[-loc,1],
             x1=datSeg[-loc,2],
             y0=datSeg[-loc,3],
             y1=datSeg[-loc,4],...)
    
    for(i in 1:length(loc)){
      segments(x0=datSeg[loc[i],1],
               x1=(datSeg[loc[i],2] + datSeg[loc[i],1])/2,
               y0=datSeg[loc[i],3],
               y1=lims[which.min(c(abs(lims[1]-datSeg[loc[i],3]), abs(lims[2]-datSeg[loc[i],3])))],...
      )
      segments(x0=(datSeg[loc[i],2] + datSeg[loc[i],1])/2,
               x1=datSeg[loc[i],2],
               y0=lims[which.min(c(abs(lims[1]-datSeg[loc[i],4]), abs(lims[2]-datSeg[loc[i],4])))],
               y1=datSeg[loc[i],4],...
      )
    }
  }else{
    segments(x0=datx[1:(length(datx)-1)],
             x1=datx[2:length(datx)],
             y0=daty[1:(length(datx)-1)],
             y1=daty[2:length(datx)],...)
  }
}
