\name{circLocs}
\alias{circLocs}

\title{
To Generate Locations on a Circle}
\description{
Generating locations on a circle 
}
\usage{
circLocs(n,d=1)
}

\arguments{
  \item{n}{
number of points on the circle you would like to generate
}
  \item{d}{
The diameter of the circle
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a matrix with a column representing the the number of the circle, a column representing the Y coordinate and a column representing the x coordinate
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
\code{\link{transBub}}
}
\examples{

locs<-circLocs(n=10,d=3)

plot(locs[,2],locs[,3])


}