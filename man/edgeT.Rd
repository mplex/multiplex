\name{edgeT}
\alias{edgeT}
\title{
Edge Table Generator
}
\description{
Produce the Edge Table of multiple relations.
}
\usage{
edgeT(x, semigroupClass)
}
\arguments{
  \item{x}{An array, usually with three dimensions of stacked matrices where the multiple relations are placed.}
  \item{semigroupClass}{(optional and logical) Return a \dQuote{\code{Semigroup}} class object?}
}
\details{
Function \code{edgeT} is to produce the \emph{Edge Table} of multiple relations in a network, which is a complete 
right multiplication table of the semigroup having its elements for each of its generators, 
}
\value{
A list of \dQuote{\code{EdgeTable}} class object. 
  \item{gens}{generator relations}
  \item{ET}{Edge Table}
}
\references{
Cannon, J.J. \dQuote{Computing the ideal structure of finite semigroup,} \emph{Numerische Mathematik}, 18, 254-266. 1971. \cr
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{wordT}}, \code{\link{semigroup}}.
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# obtain its edge table
edgeT(arr)
}
\keyword{algebra}
