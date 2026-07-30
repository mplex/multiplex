\name{write.edgel}
\alias{write.edgel}
\title{
Write edge-list files
}
\description{
A function to write edge-list files having columns for sender, receiver, and ties in multiplex networks.
}
\usage{
write.edgel(x, file = NULL, sep = "\t", header = TRUE)
}
\arguments{
  \item{x}{Array or object representing a multiple network structure.}
  \item{file}{(optional) Path with filename for output.}
  \item{sep}{(optional, default tabular) Separator between columns.}
  \item{header}{(logical) Include header in edge-list file?}
}
\details{
Write edge-list files with a \emph{s}end, \emph{r}eceive, and \emph{t}ies, which is a data frame with at least two columns for the sender and receiver, 
and the different types of tie for multiplex networks, one column for each type of relation.
}
\value{
A data frame having an edge-list format in a file with path or an object.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{edgel}}, , \code{\link{write.dl}}
}
%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
