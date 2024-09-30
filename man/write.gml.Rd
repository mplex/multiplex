\name{write.gml}
\alias{write.gml}
\title{
Write \code{gml} Files
}
\description{
A function to write files with a \code{gml} format.
}
\usage{
write.gml(x, file = NULL)
}
\arguments{
  \item{x}{
  an object representing the multiple network
  }
  \item{file}{
  path to the file
  }
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and add arguments to both the nodes and the edges for visualization purposes. 
}
\value{
A file with the data with a graph modelling language format.
}
\references{
\code{visone} \emph{Software for the analysis and visualization of social networks}. \url{http://visone.info}
}
\author{
Antonio Rivero Ostoic
}
\note{
In case that the file already exists in the pointed directory, then the file will be overwritten.
}
\seealso{
\code{\link{read.gml}}, \code{\link{write.dl}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
