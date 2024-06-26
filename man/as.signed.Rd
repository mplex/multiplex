\name{as.signed}
\alias{as.signed}
%
\title{
Coerce to a Signed Object
}
\description{
A generic function for coercing an object to a \code{Signed} class.
}
\usage{
as.signed(x, lbs)
}
%
\arguments{
  \item{x}{
	a matrix representing the signed network
}
  \item{lbs}{
	(optional) labels for the signed matrix
}
}
\details{
Since the \code{semiring} function requires an object with a `\code{Signed}' class, this function produces this class object from an array representing the signed network
}
\value{
The array as a \code{Signed} class
}
%
\seealso{
\code{\link{signed}}, \code{\link{semiring}}
}
\examples{
## Load the data
data("incubA")

## Coerce parts of the signed matrix with two types of relations
as.signed(signed(incubA$IM)$s[1:2,1:2])

}
\keyword{array}
\keyword{models}
