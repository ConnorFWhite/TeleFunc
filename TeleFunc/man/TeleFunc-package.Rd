\name{TeleFunc-package}
\alias{TeleFunc-package}
\alias{TeleFunc}
\docType{package}
\title{
Telemetry Functions
~~ TeleFunc ~~
}
\description{
Functions writted during my Masters at CSULB. Originally designed for use with acoustic telemetry data and time series data. Used for generating and plotting transistion matrices. As well as identifying events in data.
}
\details{
\tabular{ll}{
Package: \tab AssFunc\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-04-25\cr
License: \tab MIT + file LICENSE\cr
}

Focus on transMat for generating transistion matrixes and transBub for displaying the matrix

events can be used for identifying the start and end of a string of events. Designed for identifying individual dives from depth data, but can be used to identify any sort of event in time series data. functions can be applied to each event using eventFunc, or if NAs can be interpolated with eventInterp


The spackage also contains some spatial analysis converting coordinates to a cartesian frame using lat2cart, or calculating distances between two points that are in latitude and longitude

}
\author{
Connor F. White

Maintainer: <connor.white@gmail.com>


}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}

\examples{
#dat<-sample(c("A","B","C","D"),100,replace=TRUE)
#x<-transMat(dat, States=c("A","B","C","D"))

#transBub(x)


#x<-c(.5,.5,.75,.25,.2,.25,.6,.9,.1,.7)
#x.e<-event(x<.3)



}
