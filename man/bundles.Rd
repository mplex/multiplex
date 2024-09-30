\name{bundles}
\alias{bundles}
\title{
Bundle Class Patterns
}
\description{
Classify the Bundle class patterns in a system of multiple relations
}
\usage{
bundles(x, loops = FALSE, smpl = FALSE, lb2lb = TRUE, collapse = FALSE, 
        sep)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed
}
  \item{loops}{
   (logical) whether or not the loops should be considered as a particular bundle
}
  \item{smpl}{
   (logical) simplify the strings of relations? (default no)
}
  \item{lb2lb}{
   (logical) should the labels of the nodes be included in the output? (default yes)
}
  \item{collapse}{
   (logical) collapse the distinct levels of relations in the network? (default no)
}
  \item{sep}{
   (optional) pair separator used for the pairwise relations
  }
}
\details{
A bundle is a particular type of pattern made of relations at different levels that is binding a pair of nodes or actors in a network of relationships. 
A bundle class is a dyadic configuration resulting from the mixture of the direction and the types of ties between the nodes or actors. 
There are in total seven dyadic configuration classes, which are \emph{null}, \emph{asymmetric}, \emph{reciprocal}, \emph{tie entrainment}, \emph{tie exchange}, \emph{mixed}, and the \emph{full} bundle pattern. 
This function provides detailed information about the bundle class patterns in multiple networks as lists of pair relations among the nodes or actors, except for the \dQuote{\emph{null}} pattern.

In case that the nodes are not labeled, then an identification number will be assigned according to the location of the nodes in the array representation and as well when the \code{lb2lb} option is set to \code{FALSE}. 
This function assumes that the network is directed, and self ties are also considered in the output.
Long string labels are simplified with \code{smpl}, whereas the \code{collapse} option blurs the levels in the strings. 
}
\value{
An object of \sQuote{\code{Rel.Bundles}} class with the distinct bundle class patterns.
  \item{asym }{asymmetric ties}
  \item{recp }{reciprocal ties}
  \item{tent }{tie entrainment}
  \item{txch }{tie exchange}
  \item{mixed}{mixed}
  \item{full }{full}
  \item{loops }{loops (if chosen)}
}
\references{
Ostoic, J. A. R. \dQuote{Dyadic Patterns in Multiple Networks,} \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
\author{
Antonio Rivero Ostoic
}
\note{
The input array for this function is always dichotomized, and it is possible to obtain the total number of occurrences in each bundle class pattern with the \code{\link{bundle.census}} function.
}
%
\seealso{
\code{\link{bundle.census}}, \code{\link{summaryBundles}}, \code{\link{transf}}.
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

# establish the different bundles
bundles(arr)
}
\keyword{list}
\keyword{data}
\keyword{attribute}
\keyword{math}
