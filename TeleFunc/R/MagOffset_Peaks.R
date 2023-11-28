MagOffset<-function(Mag){
  ##Finding the hard iron offset
  A=cbind(Mag[,1]*2,
          Mag[,2]*2,
          Mag[,3]*2,1)
  f=matrix(Mag[,1]^2 + Mag[,2]^2 + Mag[,3]^2,ncol=1)
  
  C<-solve(crossprod(A), crossprod(A,f))
  
  rad = sqrt((C[1]^2 + C[2]^2 + C[3]^2) +C[4])
  
  return(c(C[1],C[2],C[3],rad))
}


find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}



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
