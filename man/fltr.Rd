\name{fltr}
\alias{fltr}
\title{
Principal filters
}
\description{
A function to find principal filters in a partial order
}
\usage{
fltr(x, PO, ideal = FALSE, rclos)
}
\arguments{
  \item{x}{
the reference element in the partial order (integer or character)
}
  \item{PO}{
the partial order
}
  \item{ideal}{
(logical) whether or not the ``filter'' is an ideal
}
  \item{rclos}{
(optional and logical) apply reflexive closure?
}
}
\details{
This function helps to find principal filters or principal ideals for an element in a partial order structure. Such inputs are normally a concept or an object or attribute in the concept together with the associated partial ordering structure of the concepts, which results from Galois derivations. Typically, if the reference element refers to a concept, then it is given as a positive integer indicating the concept label. Another option is to refer an object or an attribute by a character name, which should be part in the labels of the dimensions of the partial order table with a reduced labeling. Principal filters with a full labelling are not allowed if the reference element is an object or an attributes. Use an integer for the concept instead.
}
\value{
A named list with the elements in the upset or downset of the principal filter or ideal corresponding to the reference element in the partial order.
}
\references{
Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%Only \emph{principal} filters or ideals are supported for the moment.
%}
%\section{Warning }{
%
%}

\seealso{
\code{\link{galois}}, \code{\link{partial.order}}, \code{\link{diagram}}.
}
\examples{
## Create a data frame
dfr <- data.frame(x=1:3, y=5:7)

## Partial ordering of concepts
PO <- partial.order(galois(dfr),"galois")

## Filter for the first element
fltr(1, PO, rclos=TRUE)

}
\keyword{ algebra }
\keyword{ math }
