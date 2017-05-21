\name{bubcex}
\alias{bubcex}

\title{
Scaling Bubble sizes
}
\description{
Based on some input, rescale the variables so that they have these cex sizes
}
\usage{
bubcex(bubCounts, bubsize = c(1, 5), lims.b = NULL, sqrt = FALSE)
}

\arguments{
  \item{bubCounts}{
Count data representing the counts in each bubble
}
  \item{bubsize}{
a vector of two showing the minimum and maximum sizes you want
}
  \item{lims.b}{
a vector of length two representing the min and max value things will be scaled to
}
  \item{sqrt}{
TRUE/FALSE whether you would like the vector to be squareroot transformed
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a vector the same length as bubCounts representing the cex of the bubbles
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
\code{\link{transBub}}
}
\examples{

counts<-rpois(10, lambda=5)

cexs<-bubcex(bubCounts=counts)


}
