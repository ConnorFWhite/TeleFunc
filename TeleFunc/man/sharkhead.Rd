\name{sharkhead}
\alias{sharkhead}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the head on view of a shark
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sharkhead(x, y, ang = 0, exp = 1, col = "lightblue4")
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
    ylocs <- c(-0.27705, -0.26344, -0.2317, -0.17276, -0.18636, 
        -0.22263, -0.26344, -0.33, -0.23624, -0.16822, -0.11834, 
        -0.07753, 0.03582, 0.15371, 0.23986, 0.30788, 0.33509, 
        0.45751, 0.62982, 0.78852, 0.87014, 0.87014, 0.78852, 
        0.62982, 0.45751, 0.33509, 0.30788, 0.23986, 0.15371, 
        0.03582, -0.07753, -0.11834, -0.16822, -0.23624, -0.33, 
        -0.26344, -0.22263, -0.18636, -0.17276, -0.2317, -0.26344, 
        -0.27705)
    xlocs <- c(0.001213, -0.12418, -0.269817, -0.346677, -0.423537, 
        -0.528713, -0.625799, -0.75, -0.64198, -0.544894, -0.431627, 
        -0.358813, -0.338587, -0.285998, -0.217229, -0.124188, 
        -0.087781, -0.0716, -0.047329, -0.027102, 0.001213, -0.001213, 
        0.027102, 0.047329, 0.0716, 0.087781, 0.124188, 0.217229, 
        0.285998, 0.338587, 0.358813, 0.431627, 0.544894, 0.64198, 
        0.75, 0.625799, 0.528713, 0.423537, 0.346677, 0.269817, 
        0.124188, -0.001213)
    shapePlot(x = x, y = y, shapeX = xlocs, shapeY = ylocs, ang = ang, 
        exp = exp, col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
