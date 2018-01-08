\name{eventInterp}
\alias{eventInterp}

\title{
Interpret during events
}
\description{
Identify events such as strings of NA and then linearly interpolating across them
}
\usage{
eventInterp(dat,x, events, ...)
}

\arguments{
  \item{dat}{
data set that the interpolated dataset will go into
}
  \item{x}{
A logical statement in which the output is TRUE or FALSE
}
  \item{events}{
Supplied instead of x, as 2 column start and end calues
}
  \item{...}{
additionally arguments supplied to event function
}
 
}
\details{
See description of event
}
\value{
Returns a vector the same length as the original dataset, yet values are linearly interpolated where the event is true
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
event, eventMean
}
\examples{
data<-c(.5,.6,.75,NA,NA,NA,.6,.9,.8,.7)

eventInterp(dat=data,x=is.na(data))

}