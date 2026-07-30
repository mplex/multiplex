\name{hierar}
\alias{hierar}
\title{
Person and Relation Hierarchy
}
\description{
Function to establish either the Person or the Relation Hierarchy in a multiple network.
}
\usage{
hierar(W, x, type = c("person", "relation"))
}
\arguments{
  \item{W}{An object of the \dQuote{\code{Rel.Box}} class.}
  \item{x}{( or character) Actor of reference, either by the label or by an integer with its location in array \code{w} (cf. \code{\link{rbox}}).}
  \item{type}{Type of hierarchy with respect to network in \dQuote{\code{w}}: 
   \itemize{ 
    \item \code{person} for Person Hierarchy.
    \item \code{relation} for Relation Hierarchy.
   }
  }
}
\details{
The set of row inclusions in the Relation Plane of a Relation-Box \code{W} defines a partial order: the ego's \emph{Relation Hierarchy}. 
Similarly, the ego's or person's inclusions define another partial order: the ego's \emph{Person Hierarchy}. 
In other words, the Person Hierarchy captures inclusion relations among actors, whereas the Relation Hierarchy captures inclusion relations among ties. 
Both hierarchies are defined from the perspective of a selected reference actor in the network.

Notice that the Relation Plane is an outcome from \code{\link{rbox}}, whereas the cumulative person hierarchy is obtained through the \code{\link{cph}} function.
}
\value{
A list with an array that represents the partial order structure either of the Person Hierarchy in \code{ph} 
or of the Relation Hierarchy in \code{rh}. Both include the reference ego in \code{pers}.
}
\references{
Breiger, R.L. and P.E. Pattison, \dQuote{Cumulated social roles: The duality of persons and their algebras,} \emph{Social Networks}, 8, 215-256. 1986.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{rbox}}, \code{\link{cph}}, \code{\link{partial.order}}, \code{\link{diagram}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# compute person hierarchy of a random actor from relation box
arr |> 
  rbox(k=1) |> 
  hierar(ceiling(runif(1, min=0, max=dim(arr)[2])), type="person")
}
\keyword{algebra}
\keyword{math}
