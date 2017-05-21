\name{locsloc}
\alias{locsloc}

\title{
list locations
}
\description{
create a list of all possible combination of rows
}
\usage{
locsloc(locs)
}
\arguments{
  \item{locs}{
list of locations
}
}
\details{
internal function used for transition matrix things
}
\value{
martix with two columns
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
mat<-matrix(1:10,nrow=10)
locsloc(mat)

}
