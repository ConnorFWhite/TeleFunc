

lat2cart<-function(lat,long,latOrg,longOrg){
  if(is.null(latOrg)) stop("latitude of origin must be supplied")
  if(is.null(longOrg)) stop("longitude of origin must be supplied")
  pts<-cbind(lat,long)
  ptsXY<-apply(pts,1,function(z){
    y<-distGeo(z[1], longOrg,latOrg, longOrg)
    x<-distGeo(latOrg,z[2],latOrg,longOrg)
    if(z[2] < longOrg){
      x<-x*-1
    }
    if(z[1] < latOrg){
      y<-y*-1
    } 
    return(c(x,y))
  }
  )
  ptsXY<-t(ptsXY)
  colnames(ptsXY)<-c("X","Y")
  return(ptsXY)
}


distGeo<-function(lat,long,latOrg,longOrg){
  if(is.null(latOrg)) stop("Must supply 2 latitudes")
  if(is.null(longOrg)) stop("Must supply 2 longitudes")
  dlat<- pi/180*(lat-latOrg)
  dlong<- pi/180*(long-longOrg)
  a<-sin(dlat / 2) * sin(dlat / 2) +
    cos((pi / 180) * (latOrg)) * cos((pi / 180) * (lat)) *
    sin(dlong / 2) * sin(dlong / 2)
  c<-2 * atan2(sqrt(a), sqrt(1 - a))
  dist = 6371000 * c
  return(dist)
}
