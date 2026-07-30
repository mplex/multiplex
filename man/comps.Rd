\name{comps}
\alias{comps}
\title{
Find Components in Multiple Networks
}
\description{
Function to find different components and isolated nodes in the multiple networks.
}
\usage{
comps(x, bonds = c("entire", "strong", "weak"), sort)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An array representing a given network.}
  \item{bonds}{Type of bonds for components creation: 
  \itemize{
    \item \code{"entire"}: all bundle patterns
    \item \code{"strong"}: mutual bundles (reciprocal, exchange, and mixed)
    \item \code{"weak"}: asymmetrical bundles (and entraiment)
  }}
  \item{sort}{(optional and logical, default \code{FALSE}) Sort components by size?}
}
\details{
Components and isolates in network \code{x} are derived from the transitive closure of bundle ties. 
By default, the computation takes the \emph{entire} system, but the \code{bonds} option lets 
the selection of other types of relational bundles for the resulted components. 

The \code{sort} argument outputs components length in ascending size, such as dyads, triads, and 
larger groups when present. Changing the order of the existing components may have consequences in 
the layout of the graph when plotting it with function \code{\link[multigraph:multigraph]{multigraph}}.
}
\value{
A \code{list} object of length two with the \code{bonds} type in the class attribute:
  \item{com}{List or vector with network components}
  \item{isol}{Vector with network isolates}
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{bundles}}, \code{\link{summaryBundles}}, \code{\link{bundle.census}}, \code{\link{rel.sys}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# find the components and isolates
comps(arr)
}
\keyword{models}
\keyword{cluster}
