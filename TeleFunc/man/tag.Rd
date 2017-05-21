\name{tag}
\alias{tag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a small rectangle, that looks like a tag
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tag(x, y, ang = 0, exp = 1, col = "orange")
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
function (x, y, ang = 0, exp = 1, col = "orange") 
{
    xlocs <- c(-0.25, -0.25, 0.25, 0.25)
    ylocs <- c(-0.25, 0.25, 0.25, -0.25)
    shapePlot(x = x, y = y, shapeX = xlocs, shapeY = ylocs, ang = ang, 
        exp = exp, col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
