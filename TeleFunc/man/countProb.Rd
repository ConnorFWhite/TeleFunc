\name{countProb}
\alias{countProb}

\title{
Transition counts into probabilities
}
\description{
Turning transition matrix counts into state transition probabilities}
\usage{
countProb(transMat)
}

\arguments{
  \item{transMat}{
 an n by n matrix with count data
}
}
\details{
t
}
\value{
Returns an n by n matrix, with the columns representing the current state and the rows representing the probability of transition to that state. Columns will sum to 1
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F White
}
\note{
%%  ~~further notes~~
}


\seealso{
transMat
}
\examples{

rec<-sample(c("A","B","C"),size=200,replace=TRUE)
trans<-transMat(rec)

probMat<-countProb(trans)

}