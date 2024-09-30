\name{signed}
\alias{signed}
\title{
Signed Network
}
\description{
Construct the signed network of a system of contrasting relations
}
\usage{
signed(P, N = NULL, lbs)
}
\arguments{
  \item{P}{
array with the positive ties and possible with negative ties (see \sQuote{details})
}
  \item{N}{
(optional) array with the negative ties
}
  \item{lbs}{
(optional) labels for the signed matrix
}
}
\details{
This function coerces an array(s) to become a \sQuote{\code{Signed}} object. 
Positive ties are always in the first argument, and in case that this array has three dimensions, 
the second dimension is considered as the negative ties, provided that \code{N} is still \code{NULL}. 
If ambivalent ties are present in the structure then the signed matrix represent positive, negative, ambivalent, and null ties 
as \code{p}, \code{n}, \code{a}, and \code{o} respectively; otherwise, the values are \code{1}, \code{-1}, and \code{0}.

}
\value{
An object of \sQuote{\code{Signed}} class with items: 
  \item{val}{valences in the signed matrix}
  \item{s}{signed matrix}
%% ...
}
\references{
Doreian, P., V. Batagelj and A. Ferligoj  \emph{Generalized Blockmodeling}. Cambridge University Press. 2004.
}
\author{
Antonio Rivero Ostoic
}
\note{
A warning message is shown when the \code{N} argument has more than two dimensions.
}
\seealso{
\code{\link{semiring}}, \code{\link{as.signed}}
}
\examples{
# load the data
data("incubA")

# make the signed matrix with two types of relations
signed(incubA$IM)
}
%
\keyword{models}
\keyword{array}
