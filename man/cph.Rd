\name{cph}
\alias{cph}
\title{
Cumulated Person Hierarchy
}
\description{
A function to calculate the Cumulated Person Hierarchy in networks of multiple relations
}
\usage{
cph(W, lbs)
}
\arguments{
  \item{W}{
    an object of the \sQuote{\code{Rel.Box}} class.
  }
  \item{lbs}{
   (optional) the labels of the relational system
  }
}
\details{
The cumulated person hierarchy is used to determine the partial structural equivalence among the actors in a multiple network. 
Two nodes are considered as \emph{partial structural equivalent} iff they have identical role sets.

The outcome of this function depends on the characteristics of the Relation-Box.
}
\value{
An object of \sQuote{\code{Partial.Order}} class with an array representing the cumulated person hierarchy.
}
\references{
Breiger, R.L. and P.E. Pattison, \dQuote{Cumulated social roles: The duality of persons and their algebras,} \emph{Social Networks}, 8, 215-256. 1986.

Mandel, M.J. \dQuote{Roles and networks: A local approach.} B.A. Honours thesis, Harvard University. 1978.
}
\author{
Antonio Rivero Ostoic
}
\note{
If the length of the labels differs from the order of the relational system, then labels will be ignored.
}
\seealso{
\code{\link{rbox}}, \code{\link{semigroup}}, \code{\link{diagram}}
}
\examples{
# load the data
data("incubA")

# make the Relation Box of the image matrices and
# compute its cumulated person hierarchy
rbox(incubA$IM) |> 
  cph()
}
\keyword{algebra}
\keyword{math}
\keyword{array}
