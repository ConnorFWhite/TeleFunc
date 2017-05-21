\name{legend.line}
\alias{legend.line}

\title{
Add line width legend
}
\description{
a wrapper for the legend command to place the line sizes onto a transBub plot
}
\usage{
legend.line(Trans, x = NULL, y = NULL, nline = 4, lims.l = NULL, linesize = c(1, 5), sqrt = FALSE, rounddig = 2, bg = "White")
}

\arguments{
  \item{Trans}{
Transistion matrix full of count data
}
  \item{x}{
x coordinate of the plot
}
  \item{y}{
y coordinate of the plot
}
  \item{nline}{
number of lines to have in plot
}
  \item{lims.l}{
vector of length two representing the min and max counts
}
  \item{linesize}{
vector of length two representing min and max line sizes}
  \item{sqrt}{
should the count data be square root transformed
}
  \item{rounddig}{
number of digits to round numbers to
}
  \item{bg}{
color of background
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
Connor F. White
}
\note{
%%  ~~further notes~~
}

\seealso{
legend.bub, transbub
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Trans, x = NULL, y = NULL, nline = 4, lims.l = NULL, 
    linesize = c(1, 5), sqrt = FALSE, rounddig = 2, bg = "White") 
{
    if (is.null(x) | is.null(y)) {
        coords <- par("usr")
        x <- coords[1]
        y <- coords[4]
    }
    counts <- totcount(Trans)
    bubCounts <- counts$TransCount
    if (is.null(lims.l)) {
        llab <- seq(min(bubCounts[bubCounts > 0]), max(bubCounts[bubCounts > 
            0]), length.out = nline)
    }
    else {
        llab <- seq(lims.l[1], lims.l[2], length.out = nline)
    }
    lsize <- scal(llab, scal = linesize, lims = lims.l, sqrt = sqrt)
    legend(x = x, y = y, legend = round(llab, digits = rounddig), 
        lty = c(1, 1, 1, 1), lwd = lsize, bg = bg)
  }
}
