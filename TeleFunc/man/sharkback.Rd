\name{sharkback}
\alias{sharkback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the overhead view of a shark
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sharkback(x, y, ang = 0, exp = 1, col = "lightblue4")
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
    ylocs <- c(1.002, 0.988, 0.92, 0.857, 0.752, 0.643, 0.594, 
        0.494, 0.462, 0.43, 0.371, 0.321, 0.258, 0.172, 0.117, 
        0.131, 0.172, 0.185, 0.199, 0.117, 0.018, -0.159, -0.282, 
        -0.363, -0.445, -0.54, -0.604, -0.672, -0.708, -0.762, 
        -0.817, -0.894, -0.966, -1.05, -0.966, -0.894, -0.817, 
        -0.762, -0.708, -0.672, -0.604, -0.54, -0.445, -0.363, 
        -0.282, -0.159, 0.018, 0.117, 0.199, 0.185, 0.172, 0.131, 
        0.117, 0.172, 0.258, 0.321, 0.371, 0.43, 0.462, 0.494, 
        0.594, 0.643, 0.752, 0.857, 0.92, 0.988, 1.002)
    xlocs <- c(-0.002, -0.025, -0.051, -0.086, -0.103, -0.128, 
        -0.135, -0.135, -0.133, -0.171, -0.258, -0.326, -0.378, 
        -0.397, -0.371, -0.303, -0.203, -0.138, -0.132, -0.132, 
        -0.128, -0.122, -0.116, -0.112, -0.112, -0.116, -0.109, 
        -0.07, -0.051, -0.015, -0.012, -0.012, -0.006, 0, 0.006, 
        0.012, 0.012, 0.015, 0.051, 0.07, 0.109, 0.116, 0.112, 
        0.112, 0.116, 0.122, 0.128, 0.132, 0.132, 0.138, 0.203, 
        0.303, 0.371, 0.397, 0.378, 0.326, 0.258, 0.171, 0.133, 
        0.135, 0.135, 0.128, 0.103, 0.086, 0.051, 0.025, 0.002)
    shapePlot(x = x, y = y, shapeX = xlocs, shapeY = ylocs, ang = ang, 
        exp = exp, col = col)
    ylocs <- c(0.24, 0.122, 0.058, -0.06, -0.06, 0.058, 0.122, 
        0.24)
    xlocs <- c(-0.005, -0.031, -0.027, -0.004, 0.004, 0.027, 
        0.031, 0.005)
    shapePlot(x = x, y = y, shapeX = xlocs, shapeY = ylocs, ang = ang, 
        exp = exp, col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
