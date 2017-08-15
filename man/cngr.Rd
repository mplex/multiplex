\name{cngr}
\alias{cngr}
\title{
Congruence Relations
}
\description{
Find the congruence relations of a given abstract or a partially ordered semigroup.
}
\usage{
cngr(S, PO = NULL, unique = FALSE)
}
\arguments{
  \item{S}{
   an object from the `\code{Semigroup}' class.
}
  \item{PO}{
   (optional) the partial order table
}
  \item{unique}{
   (logical) whether or not return the unique congruence relations
}
}
\details{
Congruences are equivalence relations that preserve the operation between the correspondent classes in the algebraic structure. In this case the different congruence classes are based on the substitution property of the semigroup object.

}
\value{
An object of `\code{Congruence}' class.
The items included are:
  \item{S}{semigroup of relations }
  \item{PO}{partial order table (if specified)}
  \item{clu}{congruence classes}
%% ...
}
\references{
Hartmanis, J. and R.E. Stearns \emph{Algebraic Structure Theory of Sequential Machines}. Prentice-Hall. 1966.
}
\author{
Antonio Rivero Ostoic
}
\note{
If the partial order is supplied in the input, then the computation of the congruence classes is slightly faster than for an abstract semigroup.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{decomp}}, \code{\link{pacnet}} 
}
\examples{
## Create the semigroup object
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )
#
S <- semigroup(arr)

## look at the congruences in S
cngr(S, PO=NULL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ math }
\keyword{cluster}
