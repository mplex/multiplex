\name{mlvl}
\alias{mlvl}
\title{
Construct Multilevel Networks
}
\description{
Function to construct multilevel networks from multimodal structures.
}
\usage{
mlvl(x, y, type = c("bpn", "cn", "cn2"), symCdm, diag, lbs)
}
\arguments{
  \item{x}{Array or matrix with domain data.}
  \item{y}{(optional for \code{"cn"}) Dataframe or list of dataframes with codomain data associated to \code{x}.}
  \item{type}{
    Type of multilevel system to construct: 
   \itemize{ 
    \item \code{bpn} for a binomial projection
    \item \code{cn} for a common membership network
    \item \code{cn2} for a co-affiliation of network members
   }
  }
  \item{symCdm}{(optional and logical) Symmetrize codomains in binomial projection?}
  \item{diag}{(optional and logical) Include entries of the diagonal in matrices?}
  \item{lbs}{(optional) Either a list of length two with domain and codomain element labels or a vector with relation names in multilevel system.}
}
\details{
The inputs in \code{x} and \code{y} accept multiplex networks and an affiliation network that is high dimensional. 

The default multilevel system is a binomial projection \code{bpn} that requires data for the two domains, and 
where the projection is on the system codomain as a directed structure. 
Making symmetric the codomain structure with \code{symCdm} can be convenient for the visualization of 
multilevel graphs as with \code{\link[multigraph:mlgraph]{mlgraph}}, and for similar practical reasons, 
self-relations can be removed from the multilevel structure by disabling \code{diag}.

A co-affiliation of network members with option \code{cn2} requires both inputs as well, and the codomain structure 
in this case is symmetric. However, a common membership network with option \code{cn} does not need input in \dQuote{\code{x}} 
since it returns only the co-affiliation of network members from the codomain structure in \code{y} as in \code{cn2}. 

Since these are different components in the multilevel system for co-affiliation of network members, 
it is possible to specify the domain and codomain labels in \code{lbs} as a list object of length two. 
Option \code{lbs} as a vector can serve instead to label the relation types in the multilevel structure, 
which may be needed when the codomain in \code{y} is just a data frame.
}
\value{
A list object of \sQuote{\code{Multilevel}} class with the chosen type in attribute.
  \item{mlnet}{multilevel network with domain (unless \code{cn}) and projected codomain}
  \item{lbs}{(list) domain and codomain labels}
  \item{modes}{vector with either \code{1M} for domain or \code{2M} for codomain in \code{mlnet}}
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link[multigraph:mlgraph]{mlgraph}}, \code{\link[multigraph:multigraph]{multigraph}}
}
\examples{
# array for the domain
arr1 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18), c(3,3,2))>.9, 3 ) )

# rectangle array for the co-domain
arr2 <- round( replace( array(runif(12), c(3,2,2)), array(runif(12), c(3,2,2))>.9, 3 ) )

# multilevel system with default type
mlvl(arr1, arr2)
}
\keyword{models}
\keyword{data}
