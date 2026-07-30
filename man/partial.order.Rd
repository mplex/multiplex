\name{partial.order}
\alias{partial.order}
\title{
Partial Order of String Relations or of Galois Derivations
}
\description{
Construct the partial order table of unique relations of the semigroup, or else of the concepts produced by Galois derivations.
}
\usage{
partial.order(x, type = c("strings", "galois", "pi.rels"), lbs, sel, po.incl, valued)
}
\arguments{
  \item{x}{A \sQuote{\code{Strings}} or a \sQuote{\code{Galois}} class object.}
  \item{type}{Type of partial order:
  \itemize{ 
    \item \code{strings} for string relations
    \item \code{galois} for Galois derivations
    \item \code{pi.rels} for \eqn{\pi}-relations
  }}
  \item{lbs}{(optional) Labels of the unique relations.}
  \item{sel}{(optional) Selected elements in \code{x} for the partial order.}
  \item{po.incl}{(optional, only for \code{pi.rels}) Include the partial order in the \eqn{\pi}-relations?}
  \item{valued}{(optional and logical) Treat string relations in \code{x} as valued?}
}
\details{
To get the partial order of an entire semigroup, both generators and compound relations must be considered. 
This information and the labels of the unique relations are given by the \code{\link{strings}} function. 
cf. \code{\link{semigroup}} to see how the \code{x} should be specified properly.

Galois derivations are now possible to be partially ordered as well, and this option is based on the 
output given by the \code{\link{galois}} function.
}
\value{
An object of \sQuote{\code{Partial.Order}} class with the partial order table in a matrix format.
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993. \cr
Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{as.strings}}, \code{\link{strings}}, \code{\link{galois}}, \code{\link{perm}}, \code{\link{diagram}}, \code{\link{fltr}}.
}
\examples{
# load a dataset 
data("incubA")

# strings in structure and partial order
strings(incubA$IM) |> 
  partial.order()
}
\keyword{algebra}
\keyword{math}
