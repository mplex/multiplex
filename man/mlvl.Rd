\name{mlvl}
\alias{mlvl}
\title{
Construct multilevel structures
}
\description{
Function to construct multilevel structures from a one- and a two-mode network.
}
\usage{
mlvl(x = NULL, y = NULL, type = c("bpn", "cn", "cn2", "list"), 
    symCdm, diag, lbs)
}
\arguments{
  \item{x}{
  domain data
}
  \item{y}{
  codomain data
}
  \item{type}{
  type of multilevel structure
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
The default multilevel structure is \code{bpn} that requires data for the two domains. 
However, option \code{cn} does need \code{x} since returns the co-affiliation network of the codomain structure. 

Many times is convenient to specify the domain and codomain labels. 
Since these are different components in the multilevel structure, then the components' labels have to be be specified as a list object. 
}
\value{
An object of `\code{Multilevel}' class of chosen type.
  \item{mlnet}{
  the multilevel network
  }
  \item{lbs}{
  (list) domain and codomain labels
  }
  \item{modes}{
  a vector indicating the domain of the data in \code{mlnet} where \code{1M} is for domain and \code{2} is for the codomain.
  }
%%% ...
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Function may need further testing of exceptions.
}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
\code{\link[multigraph:mlgraph]{mlgraph}}, \code{\link[multigraph:multigraph]{multigraph}}
}
\examples{
# array for the domain
arr1 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18), c(3,3,2))>.9, 3 ) )

# rectangle array for the co-domain
arr2 <- round( replace( array(runif(12), c(3,2,2)), array(runif(12), c(3,2,2))>.9, 3 ) )

# multilevel structure with default type
mlvl(arr1, arr2)
}
%
\keyword{models}
\keyword{data}
