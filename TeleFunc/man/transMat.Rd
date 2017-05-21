\name{transMat}
\alias{transMat}
\title{
Transition counts
}
\description{
Create counts in the form of a matrix from one state to the next state.
}
\usage{
transMat(State_rec, prob = FALSE, States = NULL)
}

\arguments{
  \item{State_rec}{
This is a list of the currents states can be in numeric, character or factor form
}
  \item{prob}{
  This is a item that when set to true return ths probabilities instead of the counts in each cell
}
  \item{States}{
  This is a item that when given is the list of states that you would like to use, Otherwise defaults to unique(State_rec)
}
}
\details{
for plotting see transBub
}
\value{
Returns an n by n matrix, with n being the number of states. If states is not supplied, defaults to the number of unique values in State_rec. If prob is true, then these counts are transformed into probabilities, with the sum of each column equalling 1. The counts in each cell are based on the number of times a transistion from the state represented by the column to the state represented by the row occured. See transBub for plotting
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
transBub
}
\examples{

#make 100 values of blue green and yellow

dat<-sample(c("A","B","C"),100,replace=TRUE)
transMat(dat)
transMat(dat,prob=TRUE)


}

