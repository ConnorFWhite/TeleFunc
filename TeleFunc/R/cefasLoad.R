readCefas<-function(file,time=FALSE){
  
  ####Working on multiple time formats.....
  tformats<-c("%d-%m-%Y %H:%M:%S","%d-%m-%y %H:%M:%S","%d/%m/%y %H:%M:%S","%d/%m/%Y %H:%M:%S","%Y-%m-%d %I:%M:%S %p","%m-%d-%Y %I:%M:%S %p","%m/%d/%Y %I:%M:%S %p")
  
  dat<-read.table(file,col.names=paste("column", 1:10, sep="_"),fill=TRUE,sep=",")
  dat<-dat[(((grep(pattern = "Time Stamp", x = as.character(dat[,1]))[1])-6):nrow(dat)),]
  dat<-dat[,1:4]
  
  colnames(dat)<-c("DateTime","Chanel1","Chanel2", "Chanel3")
  
  blocks<-event(!is.na(as.numeric(as.character(dat$Chanel1))),ends = 2)
  
  blockInfo <- NULL
  dataOut <- list()
  for(i in 1:nrow(blocks)){
    datBl <- dat[blocks[i,1]:blocks[i,2],]
    if(time==TRUE){
      tout<-NULL
      for(ts in 1:length(tformats)){
        tout<-c(tout,as.POSIXct(as.character(datBl[1,1]),format=tformats[ts]))
      }
      if(any(!is.na(tout))){
        tformat<-tformats[which(!is.na(tout))]
        datBl[,1] <- as.POSIXct(as.character(datBl[,1]),format=tformat) 
      }else{
        tsplit<-strsplit(as.character(datBl[1,1]),split="\\.")[[1]][1]
        datBl[,1] <- as.POSIXct(tsplit,format="%Y-%m-%d %I:%M:%S %p")
      }
    }
    
    datBl[,2] <- as.numeric(as.character(datBl[,c(2)]))
    datBl[,3] <- as.numeric(as.character(datBl[,c(3)]))
    datBl[,4] <- as.numeric(as.character(datBl[,c(4)]))
    colnames(datBl) <- sapply((dat[(blocks[i,1]-1),]),FUN=as.character)
    colnames(datBl)[1]<-'Time Stamp'
    datBl <- datBl[,which(!is.na(datBl[1,]))]
    dataOut[i][[1]] <- datBl
    
    block <- as.character((dat[(blocks[i,1]-7),1]))
    var <- colnames(datBl)[2]
    rate <- ifelse(time,round(as.numeric(datBl[2,1])-as.numeric(datBl[1,1]),digits=3),NA)
    start <- as.character(datBl[1,1])
    end <- datBl[nrow(datBl),1]
    npoint <- nrow(datBl)
    
    Meta<-c(block,var,rate,start,end,npoint)
    blockInfo<-rbind(blockInfo,Meta)
  }
  names(dataOut)<-sapply(c(1:nrow(blocks)),FUN=function(x){paste("Block",x,sep="")})
  dataOut$BlockInfo<-as.data.frame(blockInfo)
  rownames(dataOut$BlockInfo)<-1:nrow(dataOut$BlockInfo)
  colnames(dataOut$BlockInfo)<-c("Block","Variable","Rate","Start","End","nPoints")
  return(dataOut)
}
