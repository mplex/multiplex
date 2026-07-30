\name{signed}
\alias{signed}
\title{
Signed Network
}
\description{
Construct the signed network of a system made of contrasting relations.
}
\usage{
signed(P, N = NULL, lbs)
}
\arguments{
  \item{P}{Array with positive ties and eventually with negative ties (see \sQuote{details}).}
  \item{N}{(optional) Array with the negative ties.}
  \item{lbs}{(optional) Labels for returned signed matrix.}
}
\details{
This function coerces an array(s) to become a \sQuote{\code{Signed}} object. 
Positive ties are always in the first argument, and in case that this array has three dimensions, 
the second dimension is considered as the negative ties, provided that \code{N} is still \code{NULL}. 

If ambivalent ties are present in the structure then the signed matrix represent positive, negative, ambivalent, and null ties 
as \code{p}, \code{n}, \code{a}, and \code{o} respectively; otherwise, the values are \code{1}, \code{-1}, and \code{0}.
}
\value{
A \sQuote{\code{Signed}} class object with items: 
  \item{val}{Valences in \code{s}}
  \item{s}{Signed matrix}
%
A warning message is shown when the \code{N} argument has more than two dimensions.
}
\references{
Doreian, P., V. Batagelj and A. Ferligoj  \emph{Generalized Blockmodeling}. Cambridge University Press. 2004.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~

\seealso{
\code{\link{semiring}}, \code{\link{as.signed}}
}
\examples{
# load a dataset
data("incubA")

# make signed matrix of image matrix in dataset
incubA |> getElement("IM") |> signed()
}
\keyword{models}
\keyword{array}
