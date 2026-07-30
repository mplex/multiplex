\name{cngr}
\alias{cngr}
\title{
Congruence Relations
}
\description{
Find the congruence relations of a given abstract semigroup or a partially ordered semigroup.
}
\usage{
cngr(S, PO, uniq)
}
\arguments{
  \item{S}{A \dQuote{\code{Semigroup}} class object.}
  \item{PO}{(optional) A partial order table.}
  \item{uniq}{(optional and logical) Return unique congruence relations?}
}
\details{
\emph{Congruencies} are equivalence relations that preserve the operation between the correspondent classes in the algebraic structure 
where the different congruence classes are based on the substitution property of the semigroup object. 

In case that the partial order is supplied in \code{PO} the computation of the congruence classes is slightly faster than for an abstract semigroup.
}
\value{
A \dQuote{\code{Congruence}} class list object with:
  \item{S}{Semigroup of relations}
  \item{PO}{Partial order table (if specified)}
  \item{clu}{Congruence classes}
}
\references{
Hartmanis, J. and R.E. Stearns \emph{Algebraic Structure Theory of Sequential Machines}. Prentice-Hall. 1966.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{decomp}}, \code{\link{fact}}, \code{\link{pacnet}} 
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# abstract semigroup of 'arr'
S <- arr |> semigroup()

# look at the congruences in S
cngr(S)
}
%
\keyword{algebra}
\keyword{math}
\keyword{cluster}
