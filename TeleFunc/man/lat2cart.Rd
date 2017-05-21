\name{lat2cart}
\alias{lat2cart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate cartesian coordinates
}
\description{
convert a series of lat and longs to a cartesian coordinate frame centered on an origin
}
\usage{
lat2cart(lat, long, latOrg, longOrg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
Latitude(s) to be converted to meters
}
  \item{long}{
Longitude(s) to be converted to meters
}
  \item{latOrg}{
  Latitude of origin
}
  \item{longOrg}{
  Longitude of origin}
}
\details{
returns the x and y location of the set of points in refernce to the origin. Distances are calculated assuming the earth is a sphere. }
\value{
 the output is in x and y coordinates from the origin. If a vector or lats and longs are supplied than a list of x and y coordinates are returned
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
distGeo
}
\examples{

#converting one location
lats<-runif(1,-90,90)
longs<-runif(1,-180,180)
latitude_origin<-0
longitude_origin<-0
lat2cart(lats,longs,latitude_origin,longitude_origin)

#Converting many locaitons at once
lats<-runif(10,-90,90)
longs<-runif(10,-180,180)
latitude_origin<-0
longitude_origin<-0
lat2cart(lats,longs,latitude_origin,longitude_origin)

}