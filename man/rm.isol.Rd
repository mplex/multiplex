\name{rm.isol}
\alias{rm.isol}
\title{
Remove Isolates
}
\description{
Function to remove isolate nodes in simple and multiple networks.
}
\usage{
rm.isol(x, diag, diag.incl)
}
\arguments{
  \item{x}{
   a matrix or array representing a network
}
  \item{diag}{
   (optional and logical) for arrays, should diagonals be included in transformation?
}
  \item{diag.incl}{
   (optional and logical) for arrays, should diagonals be included in the output?
}
}
\details{
Isolated nodes do not have any edges in the network, and in a multivariate system, there is no edges adjacent to these kinds of nodes at any level.
}
\value{
The matrix or array representing a multiple network without the isolated actors.
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
\seealso{ 
\code{\link{edgel}}, \code{\link{zbind}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
         c(3, 3, 2) ) > .5, 3 ) )

# remove isolates (if exist)
rm.isol(arr)
}
\keyword{manip}
\keyword{array}

