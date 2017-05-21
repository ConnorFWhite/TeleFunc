\name{legend.bub}
\alias{legend.bub}

\title{
Add Legend
}
\description{
To add a legend to a transbub plot, with the bubbles and the counts
}
\usage{
legend.bub(Trans, nbub = 3, xper = c(0.6, 0.95), yper = c(0.5, 0.95), sqrt = FALSE, lims.b = NULL, bubsize = c(1, 5), textl = NULL, yspacing = NULL, xspacing = NULL, boarder = 1, rounddig = 2, pch = 21, pt.bg = rgb(0, 0, 1, 0.5), pt.col = "black", bg = "white")
}

\arguments{
  \item{Trans}{
Transistion matrix
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
Connor F. White
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
transBub
}
\examples{

rec<-sample(c("A","B","C"),size=200,replace=TRUE)
trans<-transMat(rec)

transBub(trans)
legend.bub(trans,nbub=3)
}
