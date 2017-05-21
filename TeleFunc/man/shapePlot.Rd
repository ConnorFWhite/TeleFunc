\name{shapePlot}
\alias{shapePlot}

\title{
Add a polygon onto a plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shapePlot(x, y, shapeX = NULL, shapeY = NULL, ang = 0, exp = 1, col = "lightblue4")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x coordinate for center of polygon
}
  \item{y}{
y coordinate for center of polygon
}
  \item{shapeX}{
vector of the polygon x locations, perferable between -1 and 1
}
  \item{shapeY}{
vector of the polygon y locations, perferable between -1 and 1
}
  \item{ang}{
angle in degrees to rotate the shape
}
  \item{exp}{
size of the object in relative terms
}
  \item{col}{
color of the polygon
}
}
\details{
For adding simply polygons for schematics onto plots. Angles rotate the polygons around the center of the polygon. exp is a term displaying the relative size of the polygon compared to the plot surface
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor White
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

plot(0,type="n",ylim=c(-10,10),xlim=c(50,70))
shapex<-c(-1, -0.1, -1,-.1, 0, 0.1, 1, 0.1, 1, 0)
shapey<-c(-1,-.2, 0, .2, 1, .2, 0, -.2,-1,-.2 )
shapePlot(55, 5, shapeX = shapex, shapeY = shapey, ang = 0, exp = 1, col = "yellow")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
