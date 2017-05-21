\name{transBub}
\alias{transBub}

\title{
Plot Transistion Matrix
}
\description{
plot a transition matrix, from transmat. Locations can be supplied for the nodes, however, if not then the nodes are aligned in a circle.
}
\usage{
transBub(Trans, locs = NULL, bubsize = c(1, 5), lims.b = NULL, bub.pch = 21, bub.col = "black", bub.bg = rgb(0, 0, 1, 0.5), bubsqrt = FALSE, linesize = c(1, 5), lims.l = NULL, line.col = "black", bend = 0.1, head = 0.1, linesqrt = FALSE, add = FALSE, xlim = NULL, ylim = c(NULL))
}


\arguments{
  \item{Trans}{
a n x n matrix with count data representing the number of transistions from each state to each other states
}
  \item{locs}{
A three column matrix or dataframe. column one is the station names in the same order as rownames in Trans. The second column is the y coordinates, and column three is the x coordinates. if not provided data locations will be provided in a sphere
}
  \item{bubsize}{
vector of length two representing the cex parameter of the minimum and maximum node locations
}
  \item{lims.b}{
if supplied a vector of length two representing the count data representing the minimum and maximum bub sizes, else defaults to minimum and maximum observed in the data
}
  \item{bub.pch}{
Character type of the nodes
}
  \item{bub.col}{
color of the nodes
}
  \item{bub.bg}{
color of the nodes background
}
  \item{bubsqrt}{
should the count data for the nodes be square root transformed
}
  \item{linesize}{
minimum and maximum line widths
}
  \item{lims.l}{
if supplied a vector of length two representing the count data representing the minimum and maximum line widths, else defaults to minimum and maximum observed in the data
}
  \item{line.col}{
color of the lines
}
  \item{bend}{
 the amount to bend the transition lines. The porportion of distance between each node that the line will be maximumly away from the straight path. If 0 then lines are not bent
}
  \item{head}{
the length in inches of the head of the arrows, if 0 then lines are drawn with no arrow heads
}
  \item{linesqrt}{
should the transistion counts be square root transformed 
}
  \item{add}{
should theis plot be added onto the current plotting surface. If set to true, recommended that you have supplied locs
}
  \item{xlim}{
vector of length two representing the minimum and maximum x coordinates of the plot
}
  \item{ylim}{
vector of length two representing the minimum and maximum y coordinates of the plot
}
}
\details{
Can be plotted independantly. However, when locs are supplied can be added onto a map with add = true. Designed for representing these fish movements from passive acoustic telemetry
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
transMat, legend.bub, legend.line
}

\examples{
dat<-sample(c("A","B","C","D"),100,replace=TRUE)
x<-transMat(dat, States=c("A","B","C","D"))

transBub(x)

#If we want to supply locations for each node
locs<-data.frame(State=c("A","B","C","D"),y=rep(0,4),x=c(1:4))
transBub(x,locs=locs)

#If we want to supply the minimum and maximum values of the nodes
transBub(x,lims.b=c(0,40),lims.l=c(0,10))

}