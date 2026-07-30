\name{rm.isol}
\alias{rm.isol}
\title{
Remove Isolates from Network
}
\description{
Function to remove isolate nodes in simple and multiple networks.
}
\usage{
rm.isol(x, diag, diag.incl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Matrix or array representing a network.}
  \item{diag}{(optional and logical, for arrays) Include diagonals in transformation?}
  \item{diag.incl}{(optional and logical, for arrays) Include diagonals in output?}
}
\details{
Isolated nodes do not have any edges in the network, and in a multivariate system, there is no edges adjacent to these kinds of nodes at any level.
}
\value{
A matrix or an array representing a multiple network without the isolated actors.
}
%\references{} ~
%\author{} ~
%\note{} ~
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{ 
\code{\link{edgel}}, \code{\link{zbind}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
         c(3, 3, 2) ) > .5, 3 ) )

# remove isolates (if exist)
rm.isol(arr)
}
\keyword{manip}
\keyword{array}
