\name{north}
\alias{north}

\title{
add a north arrow
}
\description{
Generates and applies a north arrow to a plot
}
\usage{
north(x=NULL,y=NULL,cex=1,border="black",col="black",text= "N",
      col.text= "black",cex.text=1,text.off=.75,font=2)
}
\arguments{
  \item{x}{
 x location for arrow
}
  \item{y}{
 y location for arrow
}
  \item{cex}{
 amplifier that regulates the size of the arrow
}
  \item{border}{
 color of the border of the arrow
}
  \item{col}{
 color of the arrow
}
  \item{text}{
 text to display under the arrow
}
  \item{col.text}{
 color of the text
}
  \item{cex.text}{
 size amplified of the text
}
  \item{text.off}{
 porportion of the height of the arrow to draw the letting below the arrow
}
  \item{font}{
 type of font to use when drawing label
 }
}
\details{
just adds a simple arrow onto the plot, if x and y are not supplied the arrow is drawn in the lower left corner}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
scalebar
}
\examples{
plot(0,type="n",xlim=c(-120,-105),ylim=c(22,25))
north()
}