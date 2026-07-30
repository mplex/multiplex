\name{cph}
\alias{cph}
\title{
Cumulated Person Hierarchy
}
\description{
A function to calculate the Cumulated Person Hierarchy in networks of multiple relations. 
}
\usage{
cph(W, lbs)
}
\arguments{
  \item{W}{An object of the \dQuote{\code{Rel.Box}} class.}
  \item{lbs}{(optional) Labels of the relational system.}
}
\details{
Uses the cumulated person hierarchy to determine \emph{partial structural equivalence} among actors in a multiple network. 
Two nodes are considered partially structurally equivalent only if they have identical role sets.
Output is determined by the properties of the Relation-Box product of function \code{\link{rbox}}, and 
in case that the length of the labels differs from the order of the relational system, then labels will be ignored.
}
\value{
An object of \dQuote{\code{Partial.Order}} class with an array representing the cumulated person hierarchy.
}
\references{
Breiger, R.L. and P.E. Pattison, \dQuote{Cumulated social roles: The duality of persons and their algebras,} \emph{Social Networks}, 8, 215-256. 1986. \cr
Mandel, M.J. \dQuote{Roles and networks: A local approach.} B.A. Honours thesis, Harvard University. 1978.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{rbox}}, \code{\link{semigroup}}, \code{\link{diagram}}
}
\examples{
# load a dataset
data("incubA")

# construct the Relation Box of the dataset image matrices and
# compute its cumulated person hierarchy
incubA |> getElement("IM") |> 
  rbox() |> cph()
}
\keyword{algebra}
\keyword{math}
\keyword{array}
