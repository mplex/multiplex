\name{galois}
\alias{galois}
\title{
Galois Derivations
}
\description{
Function to compute Galois derivations between partially ordered subsets.
}
\usage{
galois(x, labeling = c("full", "reduced"), sep)
}
\arguments{\item{x}{An incidence matrix with objects and attributes.}
  \item{labeling}{Type of labeling for the Galois derivations: 
   \itemize{ 
    \item \code{full}
    \item \code{reduced}
   }
  }
  \item{sep}{(optional) Pair separator for the pairwise relations, default \dQuote{comma-space}.}
}
\details{
A \emph{Galois derivation}, or Galois connection, between the power sets of \eqn{G} and \eqn{M} is defined 
for subsets \eqn{A \subseteq G} and \eqn{B \subseteq M} by the set of attributes common to all objects in \eqn{A}, 
or \eqn{A^\prime}, and the set of objects having all attributes in \eqn{B}, or \eqn{B^\prime}.

An inclusion with the \code{"full"} labeling in the output implies first objects and then attributes in the 
Galois connection, whereas with the \code{"reduced"} option the output assumes inclusions of attributes first and then the objects. 

Notice that for \emph{many-valued} formal contexts the structure is derived from a concept scaling through function \code{\link{cscl}} 
where is possible to define a pair separator between attributes and their values. A warning message is produced when there is a 
conflict between such separators and the pair separator chosen in \code{sep} for extents and intents. 
}
\value{
A list of \dQuote{\code{Galois}} class object with Galois derivations of objects and attributes as list labels:
  \item{sep}{pair separator}
  \item{gc}{galois connections of pairs }
Notice that the object attribute class only is printed for \code{"full"} labeling, which is an intrinsic
part of the outcome for a \code{"reduced"} labeling.
}
\references{
Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
%\author{} ~
%\note{}
%\section{Warning }{....} ~
\seealso{
\code{\link{partial.order}}, \code{\link{cscl}}, \code{\link{diagram}}, \code{\link{fltr}}.
}
\examples{
# incidence matrix as a data frame
dfr <- data.frame(x=1:3, y=5:7)

# compute galois derivations 
galois(dfr)
}
\keyword{algebra}
\keyword{math}
