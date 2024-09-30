\name{fltr}
\alias{fltr}
\title{
Principal Order Filters
}
\description{
A function to find principal order filters and order ideals in a partial order
}
\usage{
fltr(x, PO, ideal = FALSE, rclos)
}
\arguments{
  \item{x}{
a reference element in the partial order (integer or character)
}
  \item{PO}{
the partial order
}
  \item{ideal}{
(logical) whether or not the \dQuote{filter} is an order ideal
}
  \item{rclos}{
(optional and logical) apply reflexive closure?
}
}
\details{
This function helps to find principal order filters or principal order ideals for an element in a partial order structure. 
Such inputs are normally a concept or an object or attribute in the concept together with the associated partial ordering structure of the concepts, which results from Galois derivations. 
Typically, if the reference element refers to a concept, then it is given as a positive integer indicating the concept label. 
Another option is to refer to an object or an attribute by a character name, which should be part of the labels of the dimensions of the partial order table with reduced labelling. 
Principal order filters with full labelling are not allowed if the reference element is an object or an attribute. Use an integer for the concept instead.
}
\value{
A named list with the elements in the upset or downset of the principal order filter or order ideal corresponding to the reference element in the partial order.
}
\references{
Ganter, B. and R. Wille \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%Only \emph{principal} filters or ideals are supported for the moment.
%}
%\section{Warning }{
%}
\seealso{
\code{\link{galois}}, \code{\link{partial.order}}, \code{\link{diagram}}.
}
\examples{
# create a data frame
dfr <- data.frame(x=1:3, y=5:7)

# partial ordering of concepts
PO <- dfr |> 
  galois() |> 
  partial.order(type="galois")

# order filter for the first element
fltr(1, PO, rclos=TRUE)
}
\keyword{algebra}
\keyword{math}
