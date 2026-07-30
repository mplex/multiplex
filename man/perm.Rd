\name{perm}
\alias{perm}
\title{
Array Permutation
}
\description{
Function to permutate a given array of relations.
}
\usage{
perm(x, clu, rev, lbs, sort)
}
\arguments{
  \item{x}{Matrix or array to permute.}
  \item{clu}{Vector of cluster for permutation.}
  \item{rev}{(optional and logical) Revert order in \code{clu}?}
  \item{lbs}{(optional) Vector of labels after permutation.}
  \item{sort}{(optional and logical) Sort array according to labels?}
}
\details{
This function performs a permutation of a given array or matrix representing relations according to a clustering vector of membership.
}
\value{
A permuted matrix or array
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{cph}}, \code{\link{partial.order}}
}
\examples{
# scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

# the permutation as an endomorphism
perm(s, clu = c(1,2,3))
}
\keyword{algebra}
\keyword{math}
\keyword{array}
\keyword{cluster}
\keyword{manip}
