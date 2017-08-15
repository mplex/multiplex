\name{galois}
\alias{galois}
\title{
Galois derivations between subsets
}
\description{
Function to perform Galois derivations between partially ordered subsets
}
\usage{
galois(x, labeling = c("full", "reduced"))
}
\arguments{
  \item{x}{
	a data frame with objects and attributes
}
  \item{labeling}{
	whether the derivations should be with full or reduced labeling
}
}
\details{
Galois derivations (or connections) are mappings between families of partially ordered subsets of elements. Such derivations are useful to analyse the structure of both subsets, whichin a social network are typically the actors and their corresponding affiliations or events, i.e. two-mode networks, or else a group of objects with a list of different attributes used in formal concept analysis.
}
\value{
A labelled list with Galois derivations of objects and attributes
}
\references{
Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}
\author{
Antonio Rivero Ostoic
}
\note{
Full labeling implies first objects and then attributes, whereas the reduced option is given the other way around.
}

\seealso{
\code{\link{partial.order}}, \code{\link{diagram}}, \code{\link{fltr}}.
}
\examples{
## Create a data frame
dfr <- data.frame(x=1:3, y=5:7)

## Find Galois derivations
galois(dfr)
}
\keyword{ algebra }
\keyword{ math }
