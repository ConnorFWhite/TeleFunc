\name{legend.Bubble}
\alias{legend.Bubble}
\title{
Generating legend
}
\description{
Adding a legend onto a plot
}
\usage{
legend.Bubble(text, pt.cex, textl = NULL, xper = c(0.6, 0.95), yper = c(0.5, 0.95), yspacing = NULL, xspacing = NULL, boarder = 1, pch = 21, pt.bg = "blue", pt.col = "black", bg = "blue")
}
\arguments{
  \item{text}{
the string for labels
}
  \item{pt.cex}{
the size of the bubbles
}
  \item{nbub}{
number of bubles to inclued
}
  \item{xper}{
a vector of length two representing the percent of the x axis to start and the percent of the plot to end the the plotting
}
  \item{yper}{
a vector of length two representing the percent of the y axis to start and the percent of the plot to end the the plotting
}
  \item{sqrt}{
Whether the count sizes should be square root transformed
}
  \item{lims.b}{
the limits to be used when scaling the bubble sizes
}
  \item{bubsize}{
The maximum and minimum bubble sizes}
  \item{textl}{
The number of text characters to in the label
}
  \item{yspacing}{
list of the spacing within the legend, should be length nbub + 1
}
  \item{xspacing}{
list of the x spacing of the legend, should be length nbub + 1

}
  \item{boarder}{
the spacing to put around the boarders
}
  \item{rounddig}{
digits to round the number to
}
  \item{pch}{
the character type for the bubles
}
  \item{pt.bg}{
color of the background for the point
}
  \item{pt.col}{
color of the point
}
  \item{bg}{
the background color of the legend
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


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (text, pt.cex, textl = NULL, xper = c(0.6, 0.95), yper = c(0.5, 
    0.95), yspacing = NULL, xspacing = NULL, boarder = 1, pch = 21, 
    pt.bg = "blue", pt.col = "black", bg = "blue") 
{
    nbub <- length(pt.cex)
    if (is.null(textl)) {
        textl <- max(sapply(text, nchar))
    }
    if (is.null(yspacing)) {
        bs <- pt.cex/2
        yspacing <- c(bs, boarder)
        yspacing[2:length(yspacing)] <- yspacing[2:length(yspacing)] + 
            bs
        yspacing[1] <- yspacing[1] + boarder
        yspacing <- cumsum(yspacing)/sum(yspacing)
        yspacing <- yspacing[1:nbub]
    }
    if (is.null(xspacing)) {
        xbub <- c(max(pt.cex))
        bs <- xbub/2
        xspacing <- c(xbub, textl, boarder)
        xspacing[2:length(xspacing)] <- xspacing[2:length(xspacing)] + 
            bs
        xspacing[1] <- xspacing[1] + boarder
        xspacing <- cumsum(xspacing)/sum(xspacing)
        xspacing <- xspacing[1:2]
    }
    coords <- par("usr")
    xdif <- (coords[2] - coords[1])
    ydif <- (coords[4] - coords[3])
    xper <- xper * xdif + coords[1]
    yper <- yper * ydif + coords[3]
    ycoord <- yper[1] + (yper[2] - yper[1]) * yspacing
    xcoord <- xper[1] + (xper[2] - xper[1]) * xspacing
    rect(xper[1], yper[1], xper[2], yper[2], col = bg)
    points(x = rep(xcoord[1], nbub), y = ycoord, cex = pt.cex, 
        pch = pch, bg = pt.bg, col = pt.col)
    text(x = rep(xcoord[2], nbub), y = ycoord, labels = text, 
        pos = 1, offset = -0.5)
  }
}
