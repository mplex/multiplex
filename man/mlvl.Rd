\name{mlvl}
\alias{mlvl}
\title{
Construct Multilevel Networks
}
\description{
Function to construct multilevel networks from multimodal structures.
}
\usage{
mlvl(x = NULL, y = NULL, type = c("bpn", "cn", "cn2", "list"), symCdm, 
     diag, lbs)
}
\arguments{
  \item{x}{
  domain data
}
  \item{y}{
  codomain data
}
  \item{type}{
  type of multilevel system: 
  \itemize{ 
    \item \code{bpn} for binomial projection
    \item \code{cn} for common membership network
    \item \code{cn2} for co-affiliation of network members
    \item \code{list} for the multimodal structures as a list
  }
}
  \item{symCdm}{
  (optional and logical, only for \code{bpn}) whether or not symmetrize the codomain structure
}
  \item{diag}{
  (optional and logical) whether or not include the entries in the diagonal matrices
}
  \item{lbs}{
  (optional, only for \code{cn2}) tie labels
}
}
\details{
The default multilevel system is a binomial projection \code{bpn} that requires data for the two domains, as with 
\code{cn2} as well. 

Option \code{cn} does not need the domain in \dQuote{\code{x}} since returns the co-affiliation of network members from the codomain structure. 

Since these are different components in the multilevel system for co-affiliation of network members, 
it is possible to specify the domain and codomain labels in \code{lbs} as a list object. 

Making symmetric the codomain structure with \code{symCdm} is many times convenient for visualization purposes.
}
\value{
An object of \sQuote{\code{Multilevel}} class of chosen type.
  \item{mlnet}{
  the multilevel network
  }
  \item{lbs}{
  (list) domain and codomain labels
  }
  \item{modes}{
  a vector indicating the domain of the data in \code{mlnet} where \code{1M} is for domain and \code{2M} is for the codomain.
  }
}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
%%
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
%
\keyword{models}
\keyword{data}
