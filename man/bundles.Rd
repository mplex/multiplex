\name{bundles}
\alias{bundles}
\title{
Bundle Class Patterns
}
\description{
Classify the Bundle class patterns in a system of multiple relations
}
\usage{
bundles(x, loops = FALSE, smpl = FALSE, lb2lb = TRUE, 
           collapse = FALSE, sep)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed. 
}
  \item{loops}{
   (logical) whether or not the loops should be considered as a particular bundle
}
  \item{smpl}{
   (logical) simplify the strings of relations? Default no.
}
  \item{lb2lb}{
   (logical) should the labels of the nodes be included in the output? Default yes.
}
  \item{collapse}{
   (logical) collapse the distinct levels of relations in the network? Default no.
}
  \item{sep}{
   (optional) the pair separator for the pairwise relations
  }
}
\details{
A bundle is a special type of pattern made of relations at different levels that is binding a pair of nodes or actors in a network of relations. A bundle class is a dyadic configuration resulting from the mixture of the direction and the types of tie between the nodes or actors. There are in total seven dyadic configuration classes, which are null, asymmetric, reciprocal, tie entrainment, tie exchange, mixed, and the full bundle pattern. This function provides the detailed information about the bundle class patterns in the multiple network as lists of pair relations among the nodes or actors, except for the `null' pattern. 
In case that the nodes are not labeled, then an identification number will be assigned according to the nodes' location in the array representation and as well when the \code{lb2lb} option is set to \code{FALSE}. This function assumes that the network is directed, and self ties can also be considered in the output.
Long string labels can be simplified with \code{smpl}, whereas the \code{collapse} option blurs the levels in the strings. The input array is always dichotomized.
}
\value{
An object of `\code{Rel.Bundles}' class with the distinct bundle class patterns.
\item{asym }{asymmetric}
\item{recp }{reciprocal}
\item{tent }{tie entrainment}
\item{txch }{tie exchange}
\item{mixed}{mixed}
\item{full }{full}
\item{loops }{loops (if chosen)}

%% ...
}
\references{
Ostoic, J. A. R. `Dyadic Patterns in Multiple Networks,' \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
\author{
Antonio Rivero Ostoic
}
\note{
It is also possible to obtain the total number of occurrences in each bundle class pattern by the \code{\link{bundle.census}} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bundle.census}}, \code{\link{summaryBundles}}, \code{\link{transf}}.
}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

## Establish the different bundles
bundles(arr)

}
\keyword{list}
\keyword{data}
\keyword{attribute}
\keyword{math}
