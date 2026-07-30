\name{mnplx}
\alias{mnplx}
\title{
Make Multiple Networks as Monoplex Structures
}
\description{
A function to transform multiple networks into a monoplex structure.
}
\usage{
mnplx(net, directed = TRUE, valued, diag, clu)
}
\arguments{
  \item{net}{A three-dimensional array to be transformed into a matrix.}
  \item{directed}{(optional and logical) Make symmetric the matrix?}
  \item{valued}{(optional and logical) Dichotomize the matrix?}
  \item{diag}{(optional and logical) Include diagonals?}
  \item{clu}{(optional) Vector with a cluster for permutation.}
}
\details{
This function is to collapse multiple types of tie of a network \code{net} recorded in an array into a matrix representation with monoplex relations. 
Other transformations on \code{net} are dichotomizing a valued network, and convert directed networks into undirected systems having or not self-relations 
depending on the value of \code{diag}. 
Moreover, the resulted matrix can be permuted with a clustering information in a vector in \code{clu} as with \code{\link{perm}}.
}
\value{
A matrix of monoplex relations.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{perm}}, \code{\link{zbind}}, \code{\link{dichot}}, \code{\link{reduc}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

# make array monoplex
mnplx(arr)
}
\keyword{manip}
\keyword{array}
