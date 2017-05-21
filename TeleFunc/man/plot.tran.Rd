\name{plot.tran}
\alias{plot.tran}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Transitions
}
\description{
Plot transistions between all points, which there is an lwd
}
\usage{
plot.tran(locs, lwd = 1, col = "black", bend = 0.05, head = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locs}{
A three column matrix or dataframe. column one is the station names in the same order as rownames in Trans. The second column is the y coordinates, and column three is the x coordinates.
}
  \item{lwd}{
the line width of each transistion. Should be length nrow(locs)^2
}
  \item{col}{
color of the transitions
}
  \item{bend}{
 the amount to bend the transition lines. The porportion of distance between each node that the line will be maximumly away from the straight path. If 0 then lines are not bent
}
  \item{head}{
the length in inches of the head of the arrows, if 0 then lines are drawn with no arrow heads
}
}
\details{
This is used in transBub, however, if you only want to view transistions or edges that this function can be used. However, you must supply the list of linewidths yourself. To determine the order examine locsloc or can calculate the number of counts using totcount command
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
transBub, transmat, totcount, locsloc
}
\examples{



locs<-circLocs(4)
locs2<-locsloc(locs)
dat<-sample(c("A","B","C","D"),100,replace=TRUE)
x<-transMat(dat, States=c("A","B","C","D"))
counts<-totcount(x)
lwds<-counts$TransCount
lwds[lwds>0]<-scal(lwds[lwds>0],scal=c(1,5))

plot(locs[locs2[,1],2]~locs[locs2[,2],3],type="n")
plot.tran(locs, lwd=lwds)


}
