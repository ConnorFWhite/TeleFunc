\name{logGen}
\alias{logGen}

\title{
Generate a state summarization
}
\description{
turn a time series of state data into a time standardized summary log file. Creates a matrix where each row is a time step and each column represents the number of a state occured within that time period
}
\usage{
logGen(state_rec, times, Time_Step, start, end, states = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_rec}{
vector of states
}
  \item{times}{
POSIX vector representing the time that each state was recorded
}
  \item{Time_Step}{
the number of seconds that the data will be binned over
}
  \item{start}{
POSIX item refering to when the log file will start
}
  \item{end}{
POSIX item refering to when the log file will end
}
  \item{states}{
a vector listing all the states to be included in the log file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with each column representing a single state. The number of columns will default to to the number of unique states, unless the states argument is supplied. Each row represents a time step, with the number of rows being dependent on the start and end argument. Each cell represents the number of times that that state occured between t and t+1.

Originally designed to determine the number of detections on acoustic receivers over time. 
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


station<-sample(c("A","B","C"),size=200,replace=TRUE)
start<-as.POSIXct("2016-04-26")
end<-start<-as.POSIXct("2016-04-28")
time<-seq(start,end,length.out=50)
time<-rep(time,4)
time<-time+rnorm(200,mean=3600)


#Generate a log file every hour
logGen(state_rec=station,times=time,Time_Step=3600,start=start,end=end)

#generate a log file ever 5 mins
logGen(state_rec=station,times=time,Time_Step=300,start=start,end=end)

#supply a list of states to possibly standardize, even if the state never offccured in this example

logGen(state_rec=station,times=time,Time_Step=3600,start=start,end=end,states=c("A","B","C","D"))

}