\name{write.edgel}
\alias{write.edgel}
\alias{write.srt}
\title{
Write edge list files
}
\description{
A function to write edge list files having columns for sender, receiver, and the ties for multiplex networks.
}
\usage{
write.edgel(x, file = NULL, sep = "\t", header = TRUE)
}
%
\arguments{
  \item{x}{
  an object representing the multiple network
}
  \item{file}{
  path to the file
}
  \item{sep}{
  the separator used between the columns
}
  \item{header}{
  (logical) whether the header should be included in the file
}
}
\details{
Write edge list files with a \emph{s}end, \emph{r}eceive, and \emph{t}ies, which is a data frame with at least two columns for the sender and receiver, 
and the different types of tie for multiplex networks, one column for each type of relation.
}
\value{
A file with the edge list format
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
\seealso{
\code{\link{edgel}}, , \code{\link{write.dl}}
}

%\examples{
%}
\keyword{IO}
\keyword{data}
\keyword{file }
