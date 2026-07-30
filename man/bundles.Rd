\name{bundles}
\alias{bundles}
\title{
Bundle Class Patterns
}
\description{
Classify bundle class patterns in a system of multiple relations.
}
\usage{
bundles(x, loops = FALSE, smpl = FALSE, lb2lb = TRUE, collapse = FALSE, sep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An array representing multiple relations.}
  \item{loops}{(logical) Also include loops pattern?}
  \item{smpl}{(logical) Simplify the strings of relations?}
  \item{lb2lb}{(logical) Should labels of the nodes be included in the output?}
  \item{collapse}{(logical) Collapse the distinct levels of relations in the network?}
  \item{sep}{(optional) Separator for the pairwise relations.}
}
\details{
A \emph{bundle} is a particular type of pattern made of relations at different levels that is binding a pair of nodes or actors in a network of relationships. 
A bundle class is a dyadic configuration resulting from the mixture of the direction and the types of ties between the nodes or actors. 
There are in total seven dyadic configuration classes, which are \emph{null}, \emph{asymmetric}, \emph{reciprocal}, \emph{tie entrainment}, \emph{tie exchange}, \emph{mixed}, and the \emph{full} bundle pattern. 
This function provides detailed information about the bundle class patterns in multiple networks as lists of pair relations among the nodes or actors, except for the \dQuote{\emph{null}} pattern.

In case that the nodes are not labeled, then an identification number will be assigned according to the location of the nodes in the array representation and as well when the \code{lb2lb} option is set to \code{FALSE}. 
This function assumes that the network is directed, and self ties are also considered in the output.
Long string labels are simplified with \code{smpl}, whereas the \code{collapse} option blurs the levels in the strings. 

The input array for this function is always dichotomized, and it is possible to obtain the total number of occurrences in each bundle class pattern with the \code{\link{bundle.census}} function.
}
\value{
An object of \dQuote{\code{Rel.Bundles}} class with the distinct bundle class patterns.
  \item{asym }{Asymmetric ties}
  \item{recp }{Reciprocal ties}
  \item{tent }{Tie Entrainment}
  \item{txch }{Tie Exchange}
  \item{mixed}{Mixed}
  \item{full }{Full}
  \item{loops}{Loops (if chosen)}
}
\references{
Ostoic, J.A.R. \dQuote{Dyadic Patterns in Multiple Networks,} \emph{Advances in Social Networks Analysis and Mining, International Conference on}, 475-481. 2011.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{bundle.census}}, \code{\link{summaryBundles}}, \code{\link{transf}}.
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.8, 3 ) )

# look at bundle patterns
bundles(arr)
}
\keyword{list}
\keyword{data}
\keyword{attribute}
\keyword{math}
