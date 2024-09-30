\name{wordT}
\alias{wordT}
%
\title{
Word Table of Relations
}
\description{
Function to produce the \emph{Word Table} of multiple relations as representation form of a semigroup of relations.
}
\usage{
wordT(x)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
}
\details{
The Word Table is a consequence of the Edge Table and the function gives a list of indexed elements in the complete semigroup. 

In terms of the Cayley graph of the semigroup (cf. \code{\link[multigraph:ccgraph]{ccgraph}}, the collection of unique relations 
(both compound and generators) are represented by nodes. 
On the other hand, the generators are edges that record the result of post-multiplying the compound relations by the generators.

The labels for the elements can be retrieved by the \code{\link{strings}} function. 
}
\value{
An object of the \sQuote{\code{WordTable}} class
  \item{gens}{
   the generator relations
}
  \item{WT}{
   the Word Table where \dQuote{\code{n}} stands for \emph{node} and \dQuote{\code{g}} stands for \emph{generator}
}

The generators do not have values in neither the \dQuote{node} nor the \dQuote{generator} of the Word table since they are not 
a product of any other element in the semigroup (cf. \sQuote{details} for the rest of the values).
}
\references{
Cannon, J.J. \dQuote{Computing the ideal structure of finite semigroup,} \emph{Numerische Mathematik}, 18, 254-266. 1971.

Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
\seealso{
\code{\link{edgeT}}, \code{\link{semigroup}}, \code{\link{strings}}, \code{\link[multigraph:ccgraph]{ccgraph}}.
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# obtain word table
wordT(arr)
}
\keyword{algebra}
%