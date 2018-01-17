\name{cph}
\alias{cph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cumulated Person Hierarchy
}
\description{
A function to calculate the Cumulated Person Hierarchy in networks of multiple relations
}
\usage{
cph(W, lbs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W}{
    an object of the `\code{Rel.Box}' class.
  }
  \item{lbs}{
   (optional) the labels of the relational system
  }
}
\details{
The cumulated person hierarchy is used to determine the partial structural equivalence among the actors in a multiple network. Two nodes are considered as \emph{partial structural equivalent} iff they have identical role sets.

The outcome of this function depends on the characteristics of the Relation-Box.
}
\value{
An object of `\code{Partial.Order}' class with an array representing the cumulated person hierarchy.
}
\references{
Breiger, R.L. and P.E. Pattison, `Cumulated social roles: The duality of persons and their algebras,' \emph{Social Networks}, 8, 215-256. 1986.

Mandel, M.J. `Roles and networks: A local approach'. B.A. Honours thesis, Harvard University. 1978.
}
\author{
Antonio Rivero Ostoic
}
\note{
If the length of the labels differ from the order of the relational system, then labels will be ignored.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rbox}}, \code{\link{semigroup}}, \code{\link{diagram}}
}
\examples{
## load the data
data("incubA")

## Make the Relation Box of the image matrices
rb <- rbox(incubA$IM)

## Calculate the cumulated person hierarchy
cph(rb)

}
\keyword{ algebra }
\keyword{ math }
\keyword{ array }
