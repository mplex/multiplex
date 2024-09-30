\name{cngr}
\alias{cngr}
\title{
Congruence Relations
}
\description{
Find the congruence relations of a given abstract or a partially ordered semigroup.
}
\usage{
cngr(S, PO = NULL, uniq)
}
\arguments{
  \item{S}{
   an object from the \sQuote{\code{Semigroup}} class.
}
  \item{PO}{
   (optional) the partial order table
}
  \item{uniq}{
   (optional and logical) whether or not return the unique congruence relations
}
}
\details{
Congruencies are equivalence relations that preserve the operation between the correspondent classes in the algebraic structure. 
In this case, the different congruence classes are based on the substitution property of the semigroup object.

}
\value{
An object of \sQuote{\code{Congruence}} class.
The items included are:
  \item{S}{semigroup of relations }
  \item{PO}{partial order table (if specified)}
  \item{clu}{congruence classes}
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
%
\seealso{
\code{\link{decomp}}, \code{\link{fact}}, \code{\link{pacnet}} 
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# record the abstract semigroup of 'arr'
S <- arr |> semigroup()

# look at the congruences in S
cngr(S, PO = NULL)
}
%
\keyword{algebra}
\keyword{math}
\keyword{cluster}
