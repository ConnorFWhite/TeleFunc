

NACA_Foil<-function(maxT,xlength,npoints=100,rot=0,xrot=0.3){
  xs<-seq(0,1,length.out=npoints/2)
  
  ys= 5*maxT * ((0.2969*(xs^(1/2))) - (0.1260*xs) - (0.3516*xs^2) + (0.2843*xs^3) - (0.1015*xs^4))
  
  ys<-c(ys,rev(ys*-1))
  xs<-c(xs,rev(xs)) 
  
  matOut<-cbind(xs-xrot,ys) %*% rbind(c(cos(rot),-sin(rot)),
                                      c(sin(rot), cos(rot)))
  colnames(matOut)<-c("x","y")
  
  matOut[,1]<-matOut[,1]*xlength
  matOut[,2]<-matOut[,2]* xlength
  return(data.frame(matOut))
}
