\name{locMap}
\alias{locMap}

\title{
generation a location dataframme
}
\description{
based on a series of states, and locations, it will generate a summarized matrix listing the lcoation of each state. Designed to generate the location of all the stations from a VEMCO detection history 
}
\usage{
locMap(state_rec, Lats, Longs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_rec}{
vector of the states
}
  \item{Lats}{
vector of y locations
}
  \item{Longs}{
vector of x locations
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a matrix with three columns. Column one is station name, column two is the y coordinate, and column three is the x coordinate
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
transBub
}
\examples{

rec<-sample(c("A","B","C"),size=200,replace=TRUE)
lats<-rec
long<-rec

lats[lats=="A"]<- 33
lats[lats=="B"]<- 34
lats[lats=="C"]<- 31

long[long=="A"]<- -118
long[long=="B"]<- 120
long[long=="C"]<- -117

locs<-locMap(rec,lats,long)


}
