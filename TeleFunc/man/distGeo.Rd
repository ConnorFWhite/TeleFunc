\name{distGeo}
\alias{distGeo}
\title{
Calculate Distance
}
\description{
calculate distance beteen lat and long }
\usage{
distGeo(lat, long, latOrg, longOrg)
}

\arguments{
  \item{lat}{
Latitude of point
}
  \item{long}{
longitude of point
}
  \item{latOrg}{
latitude of second point
}
  \item{longOrg}{
longitude of second point
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the number of meters between the two points
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F. White}
\note{
%%  ~~further notes~~
}


\seealso{
lat2Cart
}
\examples{

distGeo(33,-118,34,-118)

}
