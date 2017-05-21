\name{totcount}
\alias{totcount}

\title{
Summarize Transistion matrix
}
\description{
Summarize a transistion matrix to determine the number of times each state occured and the edges, which is the number of times the state changed
}
\usage{
totcount(Trans)
}

\arguments{
  \item{Trans}{
n x n Transistion matrix
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a list with two levels
  \item{StateCount }{the number of times each state occurs}
  \item{TransCount}{ the number of times each transition occurs, the diagonal, which represents a transistion onto itself is 0}

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
transMat
}
\examples{
dat<-sample(c("A","B","C","D"),100,replace=TRUE)
x<-transMat(dat, States=c("A","B","C","D"))
counts<-totcount(x)

counts$StateCount
counts$TransCount
}
