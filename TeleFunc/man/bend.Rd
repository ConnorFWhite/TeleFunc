\name{bend}
\alias{bend}

\title{
To Bend lines
}
\description{
Used in order to interpolate between points so that teh path of the line will be bent and not straight}
\usage{
bend(loc1, loc2, bend = 0.1, n=100)
}

\arguments{
  \item{loc1}{
       vector of length 2 of the first location
}
  \item{loc2}{
       vector of length 2 of the second location
}
  \item{bend}{
       porportion of distance between loc 1 and loc 2 to bend the line
}
  \item{n}{
       number of points
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 This function returns a matrix in which the first column is the x coordinates of n points, while the second column is the y coordinates
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F. White
}
\note{
Used to bend lines between two points, to reduce overlap
}


\seealso{
\code{\link{transBub}}
}
\examples{
first<-c(1,1)
second<- c(2,3)

l<-bend(first,second,bend=.1)

plot(first[1],first[2],xlim=c(0,3),ylim=c(0,4))
points(second[1],second[2])
lines(l[,1],l[,2])

}