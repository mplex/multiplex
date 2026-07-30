\name{fltr}
\alias{fltr}
\title{
Principal Order Filters
}
\description{
A function to find principal order filters and order ideals in a partial order structure.
}
\usage{
fltr(x, PO, ideal = FALSE, rclos)
}
\arguments{
  \item{x}{(integer or character) Reference element in the partial order.}
  \item{PO}{An array for the partial order or a \dQuote{\code{Partial.Order}} class object.}
  \item{ideal}{(logical) Is the \dQuote{filter} an order ideal?}
  \item{rclos}{(optional and logical) Apply reflexive closure to the outcome?}
}
\details{
This function identifies the principal order filter or principal order ideal of an element in a partial order. 
The input is typically either a concept, an object, or an attribute, together with the corresponding partial order 
of concepts derived from Galois derivations. If the reference element is a concept, specify it as a positive 
integer representing the concept label. Alternatively, an object or attribute may be given by its character name, 
provided it appears among the dimension labels of the partially labelled order table. 
When the reference element is an object or attribute, principal order filters with full labelling are not supported, 
and in that case use the corresponding concept number instead.
}
\value{
Returns a named list containing the elements in the upset or downset of the principal order filter or order ideal 
associated with the reference element in the partial order.
}
\references{
Ganter, B. and R. Wille \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
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
