\name{write.dat}
\alias{write.dat}
\title{
Write \code{dat} Files
}
\description{
A function to write \code{dat} files.
}
\usage{
write.dat(x, path)
}
\arguments{
  \item{x}{Array representing a multiple network structure.}
  \item{path}{Path with filename for output.}
}
\details{
\dQuote{\code{dat}} files are the format used in the \pkg{Pacnet} program inside \pkg{StOCNET}. 
In case that the input data represents a multiple network then a separate file will be produced, 
each file representing a single type of relationship in the system. 
The name of the output files depends on the object title.
}
\value{
File(s) output with adjacency matrices with a \code{.dat} format.

In case that a directory in path for output does not exist then the folder is created.
}
\references{
\pkg{StOCNET} \emph{An open software system for the advanced statistical analysis of social networks}. 
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{pacnet}}, \code{\link{write.gml}}, \code{\link{write.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
