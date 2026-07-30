\name{neighb}
\alias{neighb}
\title{
Actor or Group Neighborhood in Relational System
}
\description{
A function to find with a customized distance the neighbourhood of an actor or group of actors 
in a relational system representing a simple or multiple network.
}
\usage{
neighb(x, rs, type = c("und", "inn", "out"), k = 1, inclx = FALSE, expand)
}
\arguments{
  \item{x}{A reference actor labeled in \code{rs} or a vector with a group of actors.}
  \item{rs}{Relational system of the network.}
  \item{type}{Type of system: 
  \itemize{ 
    \item \code{und} for \emph{undirected} (default)
    \item \code{inn} for \emph{incoming} node's ties to \code{x}
    \item \code{out} for \emph{outgoing} arcs from \code{x}
  }
}
  \item{k}{The \dQuote{distance} of the neighbour nodes to \code{x} where \eqn{k=1} is for adjacent nodes.}
  \item{inclx}{(logical) Include the reference actor in the output?}
  \item{expand}{(optional and logical) Should the output be given by \code{k}, for \eqn{k>1}.}
}
\details{
The relational system in \code{rs} represents either the full multiple network of actors or the subset of relational bundles that are mutual or asymmetric. 
Accordingly, this function identifies the nodes adjacent to \sQuote{\code{x}} within the specified relational system, as well as neighbouring nodes at a user-defined path length. 
Once the longest path or chain has been reached, increasing \code{k} will not add further nodes to the graph. 
The \code{inn} and \code{out} options apply to directed networks.

Notice that the output does not differentiate in case the chosen reference actors are in different components of the relational system. 
}
\value{
Depending on \code{expand}, the output is either a vector or a list with the neighbour nodes to the reference actor(s). 
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{expos}}, \code{\link{rel.sys}}, \code{\link{bundles}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# establish the system of strong bonds
rs <- rel.sys(arr, bonds = "strong")

# obtain inmediate neighbourhood of the first node
neighb(1, rs)
}
\keyword{models}
\keyword{manip}
