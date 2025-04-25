\name{NACA_Foil}
\alias{NACA_Foil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate NACA Polygon
}
\description{
This function generates the x and y coordinates of a sysmetrical NACA foil
}
\usage{
NACA_Foil(maxT, xlength, npoints = 100, rot = 0, xrot = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maxT}{
This is the maximum thickness of the foil as a proportion of length
}
  \item{xlength}{
This is the total length of the NACA FOIL
}
  \item{npoints}{
The number of points used to generate the profile
}
  \item{rot}{
The angle the foil should be drawn at in radians. 0 means the cord length is along the X axis
}
  \item{xrot}{
This is the center of rotation for the foil when an angle other than 0 is used. It is in proportion of the cordlength
}
}
\details{
The profile is based on a 5 term equation 
 ys= 5*maxT * ((0.2969*(xs^(1/2))) - (0.1260*xs) - (0.3516*xs^2) + (0.2843*xs^3) - (0.1015*xs^4))
 }
\value{
This returns a two column data frame, which the first column is the x coordinate and teh second column is the y coordinate
}
\references{
https://en.wikipedia.org/wiki/NACA_airfoil#Five-digit_series
}
\author{
Connor White
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (maxT, xlength, npoints = 100, rot = 0, xrot = 0.3) 
{
    xs <- seq(0, 1, length.out = npoints/2)
    ys = 5 * maxT * ((0.2969 * (xs^(1/2))) - (0.126 * xs) - (0.3516 * 
        xs^2) + (0.2843 * xs^3) - (0.1015 * xs^4))
    ys <- c(ys, rev(ys * -1))
    xs <- c(xs, rev(xs))
    matOut <- cbind(xs - xrot, ys) \%*\% rbind(c(cos(rot), -sin(rot)), 
        c(sin(rot), cos(rot)))
    colnames(matOut) <- c("x", "y")
    matOut[, 1] <- matOut[, 1] * xlength
    matOut[, 2] <- matOut[, 2] * xlength
    return(data.frame(matOut))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
