\name{abacus}
\alias{abacus}
\title{
Generate an Abacus plot
}
\description{
Gener an abacus plot similar to the output from the Vemco Software VUE
}
\usage{
abacus(state_rec, times,states=NULL,labels=NULL,add=FALSE,xlim=NULL,
        tunit="month",col="black",ylab="Station",xlab="date",
        yline=4,xline=3,xcex=1.5,ycex=1.5,cex.yaxis=.75,cex.xaxis=.75,
        pch=15,format= "\%m/\%y")
}
\arguments{
    \item{state_rec}{
vector of states, normally station names or individuals
}
  \item{times}{
vector of times of each state in POSIX
}
  \item{states}{
list of all states that you wish to be included on the y axis
}
  \item{labels}{
the label that you wish displayed for each state defaults to the name of each state
}
  \item{add}{
if you would like to add points to an existing plot
}
  \item{xlim}{
vector of the min and max times of the x axis
}
  \item{tunit}{
a time unit that can be used in seq.POSIX. Used to determine the spacing of the x axis
}
  \item{format}{
How the date will be displayed on the x axis
}
  \item{col}{
The color of the abacus points
}
  \item{ylab}{
the label on the y axis
}
  \item{xlab}{
the label on the x axis
}
  \item{yline}{
the number of lines the label will be off the y axis, see mtext
}
  \item{xline}{
the number of lines the label will be off the x axis, see mtext
}
  \item{xcex}{
the cex for the x axis label
}
  \item{ycex}{
the cex for the y axis label
}
  \item{cex.yaxis}{
the cex for the y axis
}
  \item{cex.xaxis}{
the cex for the x axis
}
  \item{pch}{
the plotting character type for the symbols
}
}
\details{
This is a simple basic function for replicating VUE abacus plots. It is more flexible than VUE
}
\value{
This function returns a plot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F. White
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StateTime<-seq(as.POSIXct("2016-05-23"),as.POSIXct("2017-05-23"),by="days")
States<-sample(c("State1","State2","State3","State4"),size=366, replace=TRUE)
ind<-sample(c("Fish1","Fish2","Fish3","Fish4","Fish5","Fish6","Fish7","Fish9","Fish10"),size=366,replace=TRUE)

#Plotting by station
abacus(state_rec=States,times=StateTime,states=c("State1","State2","State3","State4","State5"))
abacus(state_rec=States,times=StateTime,states=c("State1","State2","State3","State4","State5"),col=as.factor(ind))

#plotting so the Y axis is individual
abacus(state_rec=ind,times=StateTime,col=as.factor(States))


}

