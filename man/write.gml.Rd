\name{write.gml}
\alias{write.gml}
\title{
Write \code{gml} Files
}
\description{
A function to write files with a graph modelling language (\code{gml}) format.
}
\usage{
write.gml(x, file = NULL)
}
\arguments{
  \item{x}{Array or object representing a multiple network structure.}
  \item{file}{(optional) Path with filename for output.}
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and add arguments to both the nodes and the edges for visualization purposes. 
}
\value{
A graph with modelling language format in a file with path or an object.
}
%\references{} ~
%\author{} ~
%\note{} ~
\section{Warning}{
Files with identical path and name to \code{file} will be overwritten.
}
\seealso{
\code{\link{read.gml}}, \code{\link{write.dl}}, \code{\link{write.dat}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
