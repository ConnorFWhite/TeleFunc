\name{scaleBar}
\alias{scaleBar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale Bar
}
\description{
add a scale bar onto a plot of a spatial object
}
\usage{
scaleBar(ref, max=1000,nbreaks=5, unit="m", x=NULL, y=NULL,wgs=TRUE, breaks=NULL,
                   col=c("black","white"), 
                   border="black",height=1,
                   cex=1,axis.off=1,font=1,digits=1)
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
  \item{col}{
color of the bars
}
  \item{border}{
color of the boarders of the boxes}
  \item{height}{
height of the boxes in character units
}
  \item{cex}{
size of teh text labels
}
  \item{axis.off}{
character units that the axis labels are away from teh boxes}
  \item{font}{
font of the axis units}
  \item{digits}{
number of digits to round the axis labels to
}

}
\details{
Defaults to drawing in the bottom left corner}
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
scaleBar2, xdistSpacing, north
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
