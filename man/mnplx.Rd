\name{mnplx}
\alias{mnplx}
%
\title{
Make Multiple Networks as Monoplex Structures
}
\description{
A function to transform multiple networks into a monoplex structure
}
\usage{
mnplx(net, directed = TRUE, dichot, diag, clu)
}
%
\arguments{
  \item{net}{
   a three-dimensional array to be transformed into a matrix
}
  \item{directed}{
   (optional) whether to make the matrix symmetric or not
}
  \item{dichot}{
   (optional) should the output be dichotomized?
}
  \item{diag}{
   (optional) should the diagonals be included?
}
  \item{clu}{
   (optional) a vector with the cluster for the permutation
}
}
\details{
With this function, it is possible to collapse multiple types of tie of a given network into a matrix representation with monoplex relations. 
Besides dichotomizing, directed networks can be converted into undirected systems as well with or without self-relations. 
Moreover, the resulted matrix can be permuted with a clustering information in a vector in \code{clu} as with \code{\link{perm}}.
}
\value{
A matrix of monoplex relations
}
%
\author{
Antonio Rivero Ostoic
}
%
\seealso{
\code{\link{zbind}}, \code{\link{dichot}}, \code{\link{reduc}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# make array monoplex
mnplx(arr)
}
\keyword{manip}
\keyword{array}
