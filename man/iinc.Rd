\name{iinc}
\alias{iinc}
\title{
Induced Inclusions
}
\description{
Function to establish class membership of induced inclusions given a partial order structure
}
\usage{
iinc(inc, PO, equat = FALSE, sep)
}
\arguments{
  \item{inc}{  
   an induced inclusion with a 'list' format
}
  \item{PO}{  
   matrix with the partial order structure
}
  \item{equat}{  
   (logical) should the equations be printed in the outcome?
}
  \item{sep}{  
   (optional) the pair separator for the pairwise relations
}
}
\details{
The induced inclusion function takes certain additions to the partial order, and then check whether a congruence relation is established among the relations.
}
\value{
A vector with the class membership of the elements in the structure
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{partial.order}}, \code{\link{semigroup}}, \code{\link{pacnet}}.
}
\examples{
## Load the data, and obtain the partial order
data("incubA")
po <- partial.order(strings(incubA$IM))

## Get a cluster vector given induced inclusion on the PO
iinc(transf(1-po), po)
}
\keyword{ algebra }
\keyword{ manip }
