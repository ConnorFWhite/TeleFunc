\name{shark}
\alias{shark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a profile of a shark
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shark(x, y, ang = 0, exp = 1, col = "lightblue4")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{ang}{
%%     ~~Describe \code{ang} here~~
}
  \item{exp}{
%%     ~~Describe \code{exp} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, ang = 0, exp = 1, col = "lightblue4") 
{
    xlocs <- c(-0.54, -0.695, -0.705, -0.645, -0.75, -0.745, 
        -0.535, -0.075, -0.105, -0.1, 0.075, 0.295, 0.47, 0.575, 
        0.605, 0.615, 0.535, 0.345, 0.015, -0.175)
    ylocs <- c(-0.03, -0.27, -0.225, -0.005, 0.315, 0.385, 0.03, 
        0.125, 0.245, 0.275, 0.13, 0.115, 0.09, 0.04, 0.005, 
        -0.03, -0.07, -0.11, -0.125, -0.105)
    shapePlot(x = x, y = y, shapeX = xlocs, shapeY = ylocs, ang = ang, 
        exp = exp, col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
