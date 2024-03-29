\name{neighb}
\alias{neighb}
\title{
Neighborhood of an actor or group of actors
}
\description{
A function to find the neighbourhood of an actor or group of actors with a customized distance.
}
\usage{
neighb(x, rs, type = c("und", "inn", "out"), k = 1, inclx = FALSE, expand)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the reference actor labeled in \code{rs} or a vector of several actors
}
  \item{rs}{
the relational system of the network
}
  \item{type}{
  whether the system is 
  
  - \code{und} for \emph{undirected} (default)

  - \code{inn} for \emph{incoming} node's ties to the reference actor
  
  - \code{out} for \emph{outgoing} arcs from the reference actor
}
  \item{k}{
  the ``distance'' of the neighbour nodes to the reference actor (where \code{k=1} gives the adjacent nodes)
}
  \item{inclx}{
  (logical) should the reference actor be included in the output?
}
  \item{expand}{
  (optional and logical) should the output be given by \code{k} (it only makes sense when \code{k>1})
}
}
\details{
The relational system serves to represent either the entire multiple network, or else just the relational bundles having a mutual or an asymmetric character. 
In this sense, this function detects the adjacent nodes to \code{x} according to the specified relational system, but as well the neighbours of the adjacent nodes with a customized length. 
Eventually, when the longest path or chain is reached, adding more value to \code{k} obviously will not produce more nodes in the graph system. 
Type options \code{inn} and \code{out} are for directed networks.
}
\value{
Depending on \code{expand}, the output is either a vector or a list with the neighbour nodes to the reference actor(s).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
The output does not differentiate in case the reference actors are in different components of the network.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expos}}, \code{\link{rel.sys}}, \code{\link{bundles}}

}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

## Determine the system of strong bonds
rs <- rel.sys(arr, bonds = "strong")

## the inmediate neighbourhood of the first node
neighb(1, rs)

}
\keyword{ models }
\keyword{ manip }

