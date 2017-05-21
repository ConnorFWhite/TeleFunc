\name{toUnit}
\alias{toUnit}

\title{
Convert Units
}
\description{
convert between different length units}
\usage{
toUnit(n,from = "feet", to = "meter")
}
\arguments{
  \item{n}{
number to convert
}
  \item{from}{
units to convert from
}
  \item{n}{
units to convert to
}
}
\details{
Units supplied must match one of the following
"inch","feet","yard","mile", "fathom",  "milimeter",  "centimeter",  "meter", "kilometer","in","ft","yd","mi",  "mm",  "cm",  "m", "km"
}
\value{
returns a fector of the same length as n
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor White
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
toUnit(n=12, from="in",to="ft")
toUnit(n=12, from="in",to="mile")

}
