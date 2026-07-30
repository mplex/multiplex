\name{bundle.census}
\alias{bundle.census}
\title{
Bundle Census of Multiple Networks
}
\description{
A function to perform the bundle census in multiple networks.
}
\usage{
bundle.census(x, loops = FALSE)
}
\arguments{
  \item{x}{An array representing multiple relations.}
  \item{loops}{(logical) Also include loops in the census?}
}
\details{
A \emph{bundle} is a particular type of pattern made of relations at different levels that is binding a pair of nodes or actors, 
and this function calculates the number of occurrences for each bundle class pattern in a given multiple network in \code{x}. 
The different types of relations in the network are typically recorded in a three dimensional array of stacked matrices, and
edge-list and pairwise-list of relations need to be transformed first into arrays with for example function \code{\link{transf}}.

Depending on the direction and occurrence of each possible tie, then it is possible to count with seven dyadic configuration 
classes in the census given in the output where fFunctions \code{\link{bundles}} and \code{\link{summaryBundles}} provide 
bundle class occurrences in the network with a more detailed information.
}
\value{
A table with the number occurrences in the distinct bundle class patterns occuring in \code{x}:
  \itemize{
   \item{BUNDLES} Total number of bundles in \code{x}, excluding the \emph{Null} pattern.
   \item{NULL} \emph{Null}
   \item{ASYMM} \emph{Asymmetric}
   \item{RECIP} \emph{Reciprocal}
   \item{T.ENTR} \emph{Tie Entrainment}
   \item{T.EXCH} \emph{Tie Exchange}
   \item{MIXED} \emph{Mixed} pattern
   \item{FULL} \emph{Full}
   \item{LOOP} \emph{Loops} (if \code{TRUE})
  }
}
\references{
Ostoic, J.A.R. \dQuote{Dyadic Patterns in Multiple Networks,} \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{bundles}}, \code{\link{summaryBundles}}, \code{\link{transf}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

# compute the bundle census
bundle.census(arr)
}
\keyword{data}
