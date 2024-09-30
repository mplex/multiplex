\name{edgeT}
\alias{edgeT}
%
\title{
Edge Table Generator
}
\description{
Function to produce the \emph{Edge Table} generator of multiple relations.
}
\usage{
edgeT(x)
}
%
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
}
\details{
The Edge Table is the complete right multiplication table of the semigroup having its elements for each of its generators.
}
\value{
An object of the \sQuote{\code{EdgeTable}} class
  \item{gens}{
  the generator relations
}
  \item{ET}{
  the Edge Table
}
}
\references{
Cannon, J.J. \dQuote{Computing the ideal structure of finite semigroup,} \emph{Numerische Mathematik}, 18, 254-266. 1971.

Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
%
\seealso{
\code{\link{wordT}}, \code{\link{semigroup}}.
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# obtain the edge table
edgeT(arr)
}
\keyword{algebra}
