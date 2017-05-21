\name{scal}
\alias{scal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale a vector
}
\description{
This is designed to rescale a vector, between two numbers. often designed for plotting purposes to scale valeus for point sizes
}
\usage{
scal(x, scal = c(0, 1), lims = NULL, sqrt = F, na.rm = F, limit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numbers to be rescaled
}
  \item{scal}{
vector of two listing the new minimum and maximum values
}
  \item{lims}{
values that should represent the minimum and maximum scaled value
}
  \item{sqrt}{
should the data be square root transformed
}
  \item{na.rm}{
how to handle NAs
}
  \item{limit}{
TRUE/FALSE if lims is supplied, should values smaller than the minimum lims or greater than the maximum lims be limited to the minimum and maximum scal 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of rescaled values
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

values<-rnorm(100)

#rescale the values between 0 and 1
scal(values,scal=c(0,1))

#in order to minimize influence of outliers
scal(values,scal=c(0,1), lims=c(-1,1))

#allow values outside of -1 and 1 to get bigger and smaller
scal(values,scal=c(0,1), lims=c(-1,1),limit=FALSE)


}
