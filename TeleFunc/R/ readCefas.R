

read.cefas<-function(file){
  
  dat<-read.table(file,col.names=paste("column", 1:10, sep="_"),fill=TRUE,sep=",")
  dat<-dat[(((which(dat[,1]=="Time Stamp")[1])-6):nrow(dat)),]
  dat<-dat[,1:4]
  
  colnames(dat)<-c("DateTime","Chanel1","Chanel2", "Chanel3")
  
  blocks<-event(!is.na(as.numeric(as.character(dat$Chanel1))),ends = 2)
  
  blockInfo <- NULL
  dataOut <- list()
  for(i in 1:nrow(blocks)){
    datBl <- dat[blocks[i,1]:blocks[i,2],]
    t1<-as.POSIXct(as.character(datBl[1,1]),format="%d/%m/%Y %H:%M:%S")
    if(!is.na(t1)){
      datBl[,1] <- as.POSIXct(as.character(datBl[,1]),format="%d/%m/%Y %H:%M:%S")
    }else{
      datBl[,1] <- as.POSIXct(strsplit(as.character(datBl[1,1]),split="\\.")[[1]][1],format="%m/%d/%Y %I:%M:%S %p") + seq(0,length.out=nrow(datBl),by=0.04)
    }
    datBl[,2] <- as.numeric(as.character(datBl[,c(2)]))
    datBl[,3] <- as.numeric(as.character(datBl[,c(3)]))
    datBl[,4] <- as.numeric(as.character(datBl[,c(4)]))
    colnames(datBl) <- sapply((dat[(blocks[i,1]-1),]),FUN=as.character)
    datBl <- datBl[,which(!is.na(datBl[1,]))]
    dataOut[i][[1]] <- datBl
    
    block <- as.character((dat[(blocks[i,1]-7),1]))
    var <- colnames(datBl)[2]
    rate <- strsplit(as.character((dat[(blocks[i,1]-4),1])),split=" = ")[[1]][2]
    start <- strsplit(as.character((dat[(blocks[i,1]-6),1])),split=" = ")[[1]][2]
    end <- strsplit(as.character((dat[(blocks[i,1]-5),1])),split=" = ")[[1]][2]
    npoint <- strsplit(as.character((dat[(blocks[i,1]-2),1])),split=" = ")[[1]][2]
    
    Meta<-c(block,var,rate,start,end,npoint)
    blockInfo<-rbind(blockInfo,Meta)
  }
  names(dataOut)<-sapply(c(1:nrow(blocks)),FUN=function(x){paste("Block",x,sep="")})
  dataOut$BlockInfo<-as.data.frame(blockInfo)
  colnames(dataOut$BlockInfo)<-c("Block","Variable","Rate","Start","End","nPoints")
  return(dataOut)
}

process.cefas<-function(dat){
  
}
