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
  \item{x}{
  an object representing the multiple network structure
}
  \item{path}{
  the path file for the output
}
}
\details{
`\code{dat}' files are the format used in the Pacnet program. In case that the input data represents a multiple network then a separate file will be produced, each one representing a single type of relationship in the system. The name of the output files depends on the object title.
}
\value{
File(s) with adjacency matrices with a \code{.dat} format
%% ...
}
\references{
\code{StOCNET} An open software system for the advanced statistical analysis of social networks.
%
\emph{http://www.gmw.rug.nl/~stocnet/}
}
\author{
Antonio Rivero Ostoic
}
\note{
In case that the directory in the path for the output does not exist then it will be created automatically.
}
\seealso{
\code{\link{pacnet}}, \code{\link{write.gml}}, \code{\link{write.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
