abacus<-function(state_rec, times,states=NULL,labels=NULL,add=FALSE,xlim=NULL,tunit="month", format="%m/%y",col="black",
                 ylab="Station",xlab="date",yline=4,xline=3,xcex=1.5,ycex=1.5,cex.yaxis=.75,cex.xaxis=.75,pch=15){
  length.out<-length(state_rec)
  if(is.null(states)){
    states<-unique(state_rec)
  }
  nstates<-length(states)
  order<-rep(1,length.out)
  for(i in 2:nstates){
    order[which(state_rec==states[i])]<-i
  }
  if(add==FALSE){
    if(is.null(xlim)){
      xlim<-c(min(times,na.rm=TRUE),max(times,na.rm=TRUE))
    } 
    if(is.null(labels)){
      labels<-states
    }
    ylim<-c(.5,(nstates+.5))
    
    plot(0,type="n",ylim=ylim,xlim=xlim,yaxt="n",xaxt="n",xlab=" ",ylab=" ")
    axis(2,at=c(1:nstates),labels=labels,las=2,cex.axis=cex.yaxis)
    axis.POSIXct(1, at=seq(xlim[1], xlim[2], by=tunit)
                 ,format=format,las=2,cex.axis=cex.xaxis)
    mtext(text = xlab,side = 1,line = xline, cex = xcex)
    mtext(text = ylab,side = 2,line = yline, cex = ycex)
  }
  points(order ~ times,pch=pch,col=col)
}
