\name{hierar}
\alias{hierar}
%
\title{
Person and Relation Hierarchy
}
\description{
Function to establish either the Person or the Relation Hierarchy in a multiple network
}
\usage{
hierar(W, x, type = c("person", "relation"))
}
%
\arguments{
  \item{W}{
    an object of \sQuote{\code{Rel.Box}}
}
  \item{x}{
   (integer or character) actor of reference, either by its location in the adjacency matrix or by the label.
}
  \item{type}{
   whether the hierarchy with respect to network \dQuote{\code{x}} is 
  \itemize{ 
    \item \code{person} for persons hierarchy
    \item \code{relation} for relations hierarchy
  }
}
}
\details{
The \code{person} hierarchy refers to the inclusion relations among the actors, whereas the \code{relation} hierarchy refers to the inclusion relations among the ties, 
and both are from the perspective of a chosen actor of reference in the given network.
}
\value{
An array that represents the partial order structure of the respective hierarchy.
}
\references{
Breiger, R.L. and P.E. Pattison, \dQuote{Cumulated social roles: The duality of persons and their algebras,} \emph{Social Networks}, 8, 215-256. 1986.
}
\author{
Antonio Rivero Ostoic
}
\note{
The cumulative person hierarchy is obtained through the \code{\link{cph}} function.
}
%
\seealso{
\code{\link{rbox}}, \code{\link{cph}}, \code{\link{partial.order}}, \code{\link{diagram}}
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
c(3, 3, 2) ) > .5, 3 ) )

# compute person hierarchy of a random actor from relation box
arr |> rbox(k=1) |> 
  hierar(ceiling(runif(1, min=0, max=dim(arr)[2])), type="person")
}
%
\keyword{algebra}
\keyword{math}
