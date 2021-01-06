\name{mlvl}
\alias{mlvl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing multilevel structures
}
\description{
Function to create a multilevel structure from a one- and a two-mode network.
}
\usage{
mlvl(x = NULL, y = NULL, type = c("bpn", "cn", "cn2", "list"), 
    symCdm, diag, lbs)
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  the domain data
}
  \item{y}{
  the codomain data
}
  \item{type}{
  the type of multilevel structure
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
The default multilevel structure is \code{bpn} that requires data for the two domains. However, option \code{cn} does need \code{x} since returns the co-affiliation network of the codomain structure. 

Many times is convenient to specify the domain and codomain labels, and since these are different components in the multilevel structure, then the labels have to be be specified as a list object. 
}
\value{
An object of `\code{Multilevel}' class of chosen type.
%%%  If it is a LIST, use
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
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
\code{\link[multigraph:multigraph]{multigraph}}
}
%\examples{
%
%}
%
\keyword{models}
\keyword{data}
