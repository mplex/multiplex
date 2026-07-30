\name{wordT}
\alias{wordT}
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
  \item{x}{Array with three dimensions of stacked matrices with generating relations.}
}
\details{
The Word Table is a consequence of the Edge Table (cf. \code{\link{edgeT}}) and the function gives a list of indexed elements in the complete semigroup. 

In terms of the Cayley graph of the semigroup (cf. \code{\link[multigraph:ccgraph]{ccgraph}}, the collection of unique relations 
(both compound and generators) are represented by nodes. 
On the other hand, the generators are edges that record the result of post-multiplying the compound relations by the generators.

The labels for the elements can be retrieved by the \code{\link{strings}} function. 

The generators do not have values in neither the \dQuote{node} nor the \dQuote{generator} of the Word table since they are not 
a product of any other element in the semigroup (cf. \sQuote{details} for the rest of the values).
}
\value{
A \sQuote{\code{WordTable}} class object with items:
  \item{gens}{Generator relations}
  \item{WT}{ Word Table where \dQuote{\code{n}} stands for \emph{node} and \dQuote{\code{g}} stands for \emph{generator}}
}
\references{
Cannon, J.J. \dQuote{Computing the ideal structure of finite semigroup,} \emph{Numerische Mathematik}, 18, 254-266. 1971. \cr
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{edgeT}}, \code{\link{semigroup}}, \code{\link{strings}}, \code{\link[multigraph:ccgraph]{ccgraph}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# obtain word table
wordT(arr)
}
\keyword{algebra}
