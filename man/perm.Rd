\name{perm}
\alias{perm}
%
\title{
Array Permutation
}
\description{
Function to permutate a given array of relation.
}
\usage{
perm(x, clu, rev, lbs, sort)
}
%
\arguments{
  \item{x}{
   a matrix or an array to be permuted
}
  \item{clu}{
   the cluster for the permutation
}
  \item{rev}{
   (optional and logical) whether the order in \code{clu} sholud be reverted.
}
  \item{lbs}{
   (optional) the labels after the permutation
  }
  \item{sort}{
   (optional and logical) sort array according to labels?
  }
}
\details{
This function serves to permutate an array representing relations according to a vector for the clustering membership.
}
\value{
A permuted matrix or array
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
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
