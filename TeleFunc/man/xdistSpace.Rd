\name{xdistSpace}
\alias{xdistSpace}

\title{
Calculate spaced x points
}
\description{
This Function is designed to calculate points spaced breaks distance apart from a plot. It will return a three column matrix with the first two columns corresponding to geographical locations and the third column being the distance from the first point
}
\usage{
xdistSpace(ref, max=1000,nbreaks=5, unit="m", x=NULL, y=NULL,wgs=TRUE, breaks=NULL)
}

\arguments{
  \item{ref}{
a spatial object that was plotted, whose coordinates you want the points to be in
}
  \item{max}{
the maximum distance you want
}
  \item{nbreaks}{
the number of equally spaced points you want
}
  \item{unit}{
the units that correspond to max, or breaks
}
  \item{x}{
The x location of the first point
}
  \item{y}{
the y location of the first points}
  \item{wgs}{
whether the map is in wgs 1984
}
  \item{breaks}{
if supplied max and nbreaks is ignored. This is a vector of distances that points will be returned for
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A 2 column matrix is returned. The first position is the x coordinates of the points. The second column is the y location of the points. The third column represents the distance between that point and the first point
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
scaleBar, scaleBar2
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
