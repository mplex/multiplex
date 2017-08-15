\name{partial.order}
\alias{partial.order}
\title{
The Partial Order of String relations or of Galois derivations
}
\description{
Construct the partial order table of unique relations of the semigroup, or else of the concepts produced by Galois derivations.
}
\usage{
partial.order(x, type = c("strings", "galois"), lbs, labels)
}
\arguments{
  \item{x}{
   an object of a `\code{Strings}' or a `\code{Galois}' class
}
  \item{type}{
   whether the object corresponds to strings of relations or Galois derivations
}
  \item{lbs}{
   (optional) the labels of the unique relations
}
  \item{labels}{
   same as \code{lbs} for backward compatibility only
}
}
\details{
To get the partial order of an entire semigroup, both generators and compound relations must be considered. This information and the labels of the unique relations are given by the \code{\link{strings}} function. cf. \code{\link{semigroup}} to see how the \code{x} should be specified properly.

Galois derivations are now possible to be partially ordered as well, and this option is based on the output given by the \code{\link{galois}} function.
}
\value{
An object of `\code{Partial.Order}' class with the partial order table in a matrix form.
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.

Ganter, B. and R. Wille  \emph{Formal Concept Analysis - Mathematical Foundations}. Springer. 1996.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}


\seealso{
\code{\link{as.strings}}, \code{\link{strings}}, \code{\link{galois}}, \code{\link{perm}}, \code{\link{diagram}}, \code{\link{fltr}}.
}
\examples{
## Load the data, and obtain the partial order
data("incubA")

## the strings in the structure
st <- strings(incubA$IM) 

## Get the partial order
partial.order(st)

}
\keyword{ algebra }
\keyword{ math }
%