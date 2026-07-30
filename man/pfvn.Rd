\name{pfvn}
\alias{pfvn}
\title{
Pathfinder Valued Networks and Triangle Inequality
}
\description{
A function to produce the skeleton of a valued network with the pathfinder algorithm and triangle inequality.
}
\usage{
pfvn(x, r, q)
}
\arguments{
  \item{x}{Network data, typically a valued array.}
  \item{r}{Distance parameter.}
  \item{q}{Parameter with the minimum distance between actors in the proximity matrix.}
}
\details{
Pathfinder analysis is based on a symmetric adjacency matrix or array in \code{x} representing valued networks where 
the values reflect the \dQuote{proximity} between pairs of network members. 
This function depends heavily on internal function \code{\link{cpath}} where the aim is to produce the skeleton or the salient structure of \code{x}.
The pathfinder structure is for undirected networks, whereas for directed network structures the triangle inequality principle is applied. 

Parameter \eqn{q} represents the length of all walks computed over the semiring in the produced dissimilarity matrix, 
while \eqn{r} is a distance measure computed by the \emph{Minkowski operation}.
}
\value{
  \item{max}{\eqn{\max} value of the network with the Frobenius norm}
  \item{r}{parameter \eqn{r}}
  \item{q}{parameter \eqn{q}}
  \item{Q}{salient structure of \code{x}}
  \item{Note}{A note when triangle inequality is used}
}
\references{
Schvaneveldt, R., Durso, F. and Dearholt, D., \dQuote{Network structures in proximity data,} in G. Bower, ed., \emph{The Psychology of Learning and Motivation: Advances in Research & Theory}, vol. 24, Academic Press, pp. 249-284. 1989. \cr
Batagelj, V., Doreian, P., Ferligoj, A. and Kejzar, N., \emph{Understanding Large Temporal Networks and Spatial Networks: Exploration, Pattern Searching, Visualization and Network Evolution}, Wiley. 2014.
}
%
\seealso{
\code{\link{galois}}, \code{\link{cscl}}, \code{\link[multigraph:multigraph]{multigraph}}.
}
\examples{
# create valued network data
arr <- round( array(runif(18), c(3,3,2)), array(runif(18), c(3,3,2)) ) * 10L

# pathfinder valued network of 'arr'
\dontrun{
pfvn(arr)
}
}
\keyword{models}
\keyword{manip}
