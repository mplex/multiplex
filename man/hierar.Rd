\name{hierar}
\alias{hierar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Person and Relation Hierarchy
}
\description{
A function to establish either the Person or the Relation Hierarchy in a multiple network
}
\usage{
hierar(W, x, type = c("person", "relation"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W}{
    an object of `\code{Rel.Box}'
}
  \item{x}{
   (integer or character) the actor of reference, either by its location in the adjacency matrix or by the label.
}
  \item{type}{
   whether the hierarchy with respect to network `\code{x}' is 
  \itemize{ 
    \item \code{person} for persons hierarchy
    \item \code{relation} for relations hierarchy
  }
}
}
\details{
The \code{person} hierarchy refers to the inclusion relations among the actors, whereas the \code{relation} hierarchy refers to the inclusion relations among the ties. 
Both are from the perspective of a chosen actor of reference in the given network.
}
\value{
An array that represents the partial order structure of the respective hierarchy.
}
\references{
Breiger, R.L. and P.E. Pattison, `Cumulated social roles: The duality of persons and their algebras,' \emph{Social Networks}, 8, 215-256. 1986.
}
\author{
Antonio Rivero Ostoic
}
\note{
The cumulative person hierarchy is obtained through the \code{\link{cph}} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rbox}}, \code{\link{cph}}, \code{\link{partial.order}}, \code{\link{diagram}}
}
\examples{
## Create the data: 2 binary relations among 3 elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
c(3, 3, 2) ) > .5, 3 ) )

## The relation box
rarr <- arr |> rbox(k=1)

## Calculated the person hierarchy of a random actor
hierar(rarr, ceiling(runif(1, min=0, max=dim(arr)[2])))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ math }
