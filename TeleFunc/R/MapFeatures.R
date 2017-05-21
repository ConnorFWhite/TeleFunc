toUnit<-function(n,from = "feet", to = "meter"){
  units<-c("inch","feet","yard","mile","Fathom",  "milimeter",  "centimeter",  "meter", "kilometer",
           "in","ft","yd","mi",  "mm",  "cm",  "m", "km")
  vals<-c(1,  0.083333,  0.0277777,  0.000015783, 0.01388889, 25.4,  2.54,  0.0254,  0.0000254,
          1,  0.083333,  0.0277777,  0.000015783, 25.4,  2.54,  0.0254,  0.0000254)
  fromloc<-which(units==from)
  toloc<-which(units==to)
  
  n<-vals[toloc]/vals[fromloc]*n
  return(n)
  
}

north<-function(x=NULL,y=NULL,cex=1,border="black",col="black",text= "N",col.text= "black",
                cex.text=1,text.off=.75,font=2){
  coords<-par("usr")
  xdif<-(coords[2]-coords[1])
  ydif<-(coords[4]-coords[3])
  
  if(is.null(x) | is.null(y)){
    x<-coords[1] + xdif*.1
    y<-coords[3] + ydif*.1
  }

  xdif<-xdif * .05 * cex
  ydif<-ydif * .1 * cex
  
  xtext<- x 
  ytext<- y - (ydif*text.off) - (ydif * -0.5)
  
  polygon(x = x+xdif*c(0, -0.5, 0, 0.5), y = y+ydif*c(0.5, -0.5, -0.2, -0.5),col=col,border=border)
  text(x = xtext, y= ytext,labels = text,cex = cex.text,font=font,col=col.text,pos=1)
}


xdistSpace<-function( x=NULL, y=NULL, max=1000,nbreaks=5, unit="m", breaks=NULL) {

  coords<-par("usr")
  xdif<-(coords[2]-coords[1])
  ydif<-(coords[4]-coords[3])
  
  orgX<-(coords[1]+coords[2])/2
  orgY<-(coords[3]+coords[4])/2
  
  if(is.null(breaks)){
    breaks<-seq(0,max,length.out=nbreaks)
  }else{
    nbreaks<-length(breaks)
    max<-max(breaks)
  }
  
  breaks2<-toUnit(breaks,from=unit,to="m")

  if(is.null(x) | is.null(y)){
    x<-coords[1] + xdif*.2
    y<-coords[3] + ydif*.1
  }
  
  start<-lat2cart(lat = y,long = x,latOrg = orgY, longOrg = orgX)
  
  xloc<-start[1,1]+breaks2
  yloc<-rep(start[1,2],nbreaks)

  out<-project(cbind(xloc,yloc),
              proj= paste("+proj=lcc +lon_0=", orgX, " +lat_0=",orgY,
                          "+datum=WGS84 +units=m", sep=""),
              inv=TRUE)
  
  out<-cbind(out,breaks)
  return(out)
}
  
  
  
  
plot.scaleBar<-function(xy, unit="m", col=c("black","white"), 
                  border="black",height=1,
                  cex=1,axis.off=1,font=1,digits=1){
  Hig<-strheight(s = "1")
  
  nbreaks<-nrow(xy)
  col<-rep(col,length.out=nbreaks)
  border=rep(border,length.out=nbreaks)
    
  for(i in 1:(nbreaks-1)){
    polygon(x = c(xy[i,1],xy[(i+1),1],xy[(i+1),1],xy[i,1]),
            y = c(xy[i,2] + (Hig*height),
                  xy[i,2] + (Hig*height),
                  xy[i,2],
                  xy[i,2]),
            border=border[i],col=col[i])
  }
  
  labs<-xy[,3]
  labs<-round(labs,digits=digits)
  labs[nbreaks]<-paste(labs[nbreaks], unit,sep= "")
  
  text(x=xy[,1],
       y=xy[,2] - (Hig*axis.off),
       labels = labs,cex=cex,font=font)
}


plot.scaleBar2<-function(xy,unit="m",
                         cex=1,las=1,lwd=1,font=1,axis.off=1,
                         col="black",digits=1){
  coords<-par("usr")
  xdif<-(coords[2]-coords[1])
  ydif<-(coords[4]-coords[3])
  
  nbreaks<-nrow(xy)

  labs<-xy[,3]
  labs<-round(labs,digits=digits)
  labs[nbreaks]<-paste(labs[nbreaks], unit,sep= "")
  
  axis(side=1,at = xy[,1], labels = labs,pos = xy[1,2],col=col,lwd=lwd, las=las,
       cex.axis=cex, font=font,mgp=c(3,axis.off,0))

}

scaleBar<-function(x=NULL, y=NULL, max=1000,nbreaks=5, unit="m",breaks=NULL,
                  col=c("black","white"), 
                   border="black",height=1,
                   cex=1,axis.off=1,font=1,digits=1){
  
  xy<-xdistSpace(unit=unit, max=max,nbreaks=nbreaks, x=x, y=y, breaks=breaks)

  plot.scaleBar(xy=xy, unit=unit,
                col=col, 
                border=border,height=height,
                cex=cex,axis.off=axis.off,font=font,digits=digits)
}


scaleBar2<-function(max=1000,nbreaks=5, unit="m", x=NULL, y=NULL,wgs=TRUE, breaks=NULL,
                    cex=1,las=1,lwd=1,font=1,axis.off=1,
                    col="black",digits=1){
  xy<-xdistSpace(x=x, y=y, unit=unit, max=max,nbreaks=nbreaks, breaks=breaks)
  plot.scaleBar2(xy,unit=unit,
                  cex=cex,las=las,lwd=lwd,font=font,axis.off=axis.off,
                  col=col,digits=digits)
}


