
locsloc<-function(locs){
  locs_loc<-cbind(rep(1:nrow(locs),nrow(locs)),
                  rep(1:nrow(locs),each=nrow(locs)))
  return(locs_loc)
}
  
totcount<-function(Trans){
  locs_loc<-locsloc(Trans)
  
  detections<-rowSums(Trans)

  trans<-Trans[locs_loc]
  trans[which(locs_loc[,1]==locs_loc[,2])]<-0
  
  return(list(StateCount=detections,TransCount=trans))
}


bubcex<-function(bubCounts,bubsize=c(1,5),lims.b=NULL,sqrt=FALSE){
  bubCounts[bubCounts>0]<-scal(bubCounts[bubCounts>0],scal=bubsize,lims=lims.b,sqrt=sqrt)
  return(bubCounts)
}


transLwd<-function(lineCounts,linesize=c(1,5),lims.l=NULL,sqrt=FALSE){
  lineCounts[lineCounts>0]<-scal(lineCounts[lineCounts>0],scal=linesize,lims=lims.l,sqrt=sqrt)
  return(lineCounts)
}

circLocs<-function(n,d=1){
  seqs<-seq((pi/2),(2*pi + (pi/2) - (2*pi)/n), length.out=n)
  
  xx <-d*cos( seqs )
  yy <-d*sin( seqs )
  locs<-data.frame(Name=c(1:n),y=yy,x=xx)
  return(locs)
}

bend<-function(loc1,loc2,bend=.1,n=100){
  seqs<-seq(0,pi,length.out=n)
  
  angle<-c(loc2[1]-loc1[1],loc1[2]-loc2[2])
  dist<-sqrt((angle[1]^2) + (angle[2]^2))
  angle<-atan2(angle[2],angle[1])
  
  bend<-bend*dist
  xoff<-sin(angle)*bend*sin(seqs)
  yoff<-cos(angle)*bend*sin(seqs)
  
  
  xs<-seq(loc1[1],loc2[1],length.out=n)
  ys<-seq(loc1[2],loc2[2],length.out=n) 
  
  yoff<-ys + yoff
  xoff<-xs + xoff
  return(data.frame(x=xoff,y=yoff))
}


plot.null<-function(locs,dect=NULL,xlim=NULL,ylim=NULL){
  if(is.null(xlim)){
    xr<-max(locs[,3])-min(locs[,3])
    xr<-xr*.05
    xlim=c((min(locs[,3])-xr),(max(locs[,3])+xr))
  }
  if(is.null(ylim)){
    yr<-max(locs[,2])-min(locs[,2])
    yr<-yr*.05
    ylim=c((min(locs[,2])-yr),(max(locs[,2])+yr))
  }
  if(is.null(dect)){
    plot(locs[,2]~locs[,3],type="n",ylab=" ",xlab= " ",
         xlim=xlim,ylim=ylim, yaxt="n",xaxt="n")
  } else{
     plot(locs[(dect>0),2]~locs[(dect>0),3],type="n",ylab=" ",xlab= " ",
       xlim=xlim,ylim=ylim, yaxt="n",xaxt="n")
  }
}


plot.bub<-function(locs,size=1,pch=21,col="black",bg="lightBlue"){
  points(locs[,2]~locs[,3],cex=size,pch=pch,bg=bg,col=col)
}


plot.tran<-function(locs, lwd=1, col="black",bend = .05, head=.1){
  locs_loc<-locsloc(locs)
  locs_loc<-locs_loc[which(lwd>0),]
  lwd<-lwd[which(lwd>0)]
  for(i in 1: nrow(locs_loc)){
    l<-bend(c(locs[locs_loc[i,1],3], locs[locs_loc[i,1],2]),
            c(locs[locs_loc[i,2],3], locs[locs_loc[i,2],2]), bend=bend)
    l<-l[15:90,]
    lines(l[,2]~l[,1],lwd=lwd[i],col=col)
    arrows(x0 = l[2,1],x1 = l[1,1],
           y0 = l[2,2],y1 = l[1,2],lwd=lwd[i],col=col,length=head)
  }
}



plot.transmat<-function(locs,
                        bub.pch=21,bub.cex=1,bub.col="black",bub.bg=rgb(0,0,1,.5),
                        line.lwd=1,line.col="black",bend=.1,head=.1){
  plot.bub(locs,size=bub.cex,pch=bub.pch,col=bub.col,bg=bub.bg)
  plot.tran(locs,lwd=line.lwd,col=line.col,bend=bend,head=head)
  
}


transBub<-function(Trans,locs=NULL,
                   bubsize=c(1,5),lims.b=NULL,bub.pch=21,bub.col="black",bub.bg=rgb(0,0,1,.5),bubsqrt=FALSE,
                   linesize=c(1,5),lims.l=NULL,line.col="black",bend=.1,head=.1,linesqrt=FALSE,
                   add=FALSE,xlim=NULL,ylim=c(NULL)){
  if(is.null(locs)){
    locs<-circLocs(n=ncol(Trans))
  }
  if(add==FALSE){
    plot.null(locs,xlim=xlim,ylim=ylim)
  }
  counts<-totcount(Trans)
  bub.cex<-bubcex(counts$StateCount,bubsize=bubsize,lims.b=lims.b,sqrt=bubsqrt)
  line.lwd<-transLwd(counts$TransCount,linesize=linesize,lims.l=lims.l,sqrt=linesqrt)
  
  plot.transmat(locs=locs,
                bub.cex=bub.cex, bub.col=bub.col, bub.bg=bub.bg, bub.pch=bub.pch,
                line.lwd=line.lwd,line.col=line.col,bend=bend,head=head)
  
}



legend.Bubble<-function(text,pt.cex,textl=NULL,xper=c(.6,.95),yper=c(.50,.95),
                     yspacing=NULL,xspacing=NULL, boarder=1,
                     pch=21,pt.bg="blue", pt.col="black", bg="blue"){
  
  nbub<-length(pt.cex)
  
  if(is.null(textl)){
    textl<-max(sapply(text,nchar))
  }
  
  if(is.null(yspacing)){
    bs<-pt.cex/2
    yspacing<-c(bs,boarder)
    yspacing[2:length(yspacing)]<-yspacing[2:length(yspacing)]+bs
    yspacing[1]<-yspacing[1]+boarder
    yspacing<-cumsum(yspacing)/sum(yspacing)
    yspacing<-yspacing[1:nbub]
  }
  if(is.null(xspacing)){
    xbub<-c(max(pt.cex))
    bs<-xbub/2
    xspacing<-c(xbub,textl,boarder)
    xspacing[2:length(xspacing)]<-xspacing[2:length(xspacing)]+bs 
    xspacing[1]<-xspacing[1]+boarder
    xspacing<-cumsum(xspacing)/sum(xspacing)
    xspacing<-xspacing[1:2]
  }
  
  coords<-par("usr")
  
  xdif<-(coords[2]-coords[1])
  ydif<-(coords[4]-coords[3])
  
  xper<-xper*xdif + coords[1]
  yper<-yper*ydif + coords[3]
  
  ycoord<-yper[1] + (yper[2] - yper[1])*yspacing
  xcoord<-xper[1] + (xper[2] - xper[1])*xspacing
  
  rect(xper[1],yper[1],xper[2],yper[2],col=bg)
  points(x = rep(xcoord[1],nbub), y = ycoord,cex=pt.cex,pch=pch,bg=pt.bg,col=pt.col)
  text(x = rep(xcoord[2],nbub),y = ycoord, labels = text,pos=1,offset=-.5)
  
}


legend.bub<-function(Trans,nbub=3,xper=c(.6,.95),yper=c(.50,.95),
                     sqrt=FALSE,lims.b=NULL,bubsize=c(1,5),textl=NULL,
                     yspacing=NULL,xspacing=NULL, boarder=1,rounddig=2,
                     pch=21,pt.bg=rgb(0,0,1,.5), pt.col="black", bg="white"){
  counts<-totcount(Trans)
  bubCounts<-counts$StateCount
  if(is.null(lims.b)){
    blab<-seq(min(bubCounts[bubCounts>0]),max(bubCounts[bubCounts>0]),length.out=nbub)
  }else{
    blab<-seq(lims.b[1],lims.b[2],length.out=nbub)
  }
  bsize<-scal(blab,scal=bubsize,lims=lims.b,sqrt=sqrt)
  
  legend.Bubble(text = round(blab,digits = rounddig),pt.cex = bsize,textl=textl,xper=xper,yper=yper,
                yspacing=yspacing,xspacing=xspacing, boarder=boarder,
                pch=pch,pt.bg=pt.bg, pt.col=pt.col, bg=bg)
}



legend.line<-function(Trans, x=NULL, y=NULL, nline=4, lims.l=NULL,linesize=c(1,5),sqrt=FALSE,rounddig=2,bg="White"){
  if(is.null(x) | is.null(y)){
    coords<-par("usr")
    x<-coords[1]
    y<-coords[4]
  }
  counts<-totcount(Trans)
  bubCounts<-counts$TransCount
  if(is.null(lims.l)){
    llab<-seq(min(bubCounts[bubCounts>0]),max(bubCounts[bubCounts>0]),length.out=nline)
  }else{
    llab<-seq(lims.l[1],lims.l[2],length.out=nline)
  }
  lsize<-scal(llab,scal=linesize,lims=lims.l,sqrt=sqrt)
  
  legend(x=x,y=y,legend = round(llab,digits = rounddig),lty=c(1,1,1,1),lwd=lsize,bg=bg)
}





