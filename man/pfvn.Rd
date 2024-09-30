\name{pfvn}
\alias{pfvn}
%
\title{
Pathfinder Valued Networks and Triangle Inequality
}
\description{
A function to establish the skeleton of a valued network with the pathfinder algorithm and triangle inequality
}
\usage{
pfvn(x, r, q)
}
%
\arguments{
  \item{x}{
  network data, typically valued
}
  \item{r}{
  a distance function parameter 
}
  \item{q}{
  parameter with the minimum distance between actors in the proximity matrix
}
}
\details{
The Pathfinder structure is for undirected networks, whereas for directed network structures the triangle inequality principle is applied

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{max}{max value of the network with the Frobenius norm}
  \item{r}{parameter \emph{r}}
  \item{q}{parameter \emph{q}}
  \item{Q}{salient structure of \emph{x}}
  \item{Note}{A note when triangle inequality is used}%% ...
}
\references{
Schvaneveldt, R., Durso, F. and Dearholt, D., \dQuote{Network structures in proximity data,} in G. Bower, ed., \emph{The psychology of learning and motivation: Advances in research & theory}, Vol. 24, Academic Press, pp. 249-284. 1989.

Batagelj, V., Doreian, P., Ferligoj, A. and Kejzar, N., \emph{Understanding Large Temporal Networks and Spatial Networks: Exploration, Pattern Searching, Visualization and Network Evolution}, Wiley. 2014.
}
\author{
Antonio Rivero Ostoic
}
%
%

\seealso{
\code{\link[multigraph:multigraph]{multigraph}}, 
}
\examples{
# create valued network data
arr <- round( array(runif(18), c(3,3,2)), array(runif(18), c(3,3,2)) ) * 10L

# pathfinder valued network of 'arr'
pfvn(arr)
}
\keyword{models}
\keyword{manip}
