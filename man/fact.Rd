\name{fact}
\alias{fact}
\title{
Factorize Partially Ordered Semigroups
}
\description{
A function to factorize partially ordered semigroup structures by induced inclusions.
}
\usage{
fact(S, P, uniq = TRUE, fac, atoms, mca, atmc, patm, k)
}
\arguments{
  \item{S}{A \dQuote{\code{Semigroup}} class object.}
  \item{P}{A \dQuote{\code{Partial.Order}} \dQuote{\code{strings}} class object associated to \code{S}.}
  \item{uniq}{(logical) Should the factorization include unique induced inclusions?}
  \item{fac}{(optional) The \sQuote{factor} to be factorized for partially ordered structures.}
  \item{atoms}{(optional and logical) Short for including \code{"mca"} and \code{"atmc"} in the output.}
  \item{mca}{(optional and logical) Include pairs of meet-complements of pair of atoms in output?}
  \item{atmc}{(optional and logical) Include meet-complements of pair of atoms in output?}
  \item{patm}{(optional and logical) Include potential atoms in output?}
  \item{k}{(optional) Integer with length of induced inclusion.}
}
\details{
\emph{Factorization} is a subdirect-representation-based decomposition technique for a partially ordered semigroup. 
This procedure decomposes the algebraic structure into simpler components, called \emph{factors}, each of which is 
a semigroup factor representing a specific homomorphic image of the algebra. 

For partially ordered semigroups, these factors are isotone homomorphic images corresponding to maximally independent 
features of the original semigroup of relations. 
The quotient semigroups and the reduced partial order tables together form the factorizing set, which 
is given by the decomposition with \code{\link{decomp}}.
}
\value{
A list of \dQuote{\code{Ind.incl}} class object having:
  \item{po}{partial order table}
  \item{iin}{list of induced inclusions pairwise listed}
  \item{niin}{length of induced inclusions}
  \item{patm}{(for \code{patm}) a vector with potential atoms}
  \item{atm}{vector with atoms}
  \item{atmc}{(for \code{atmc}) array with meet-complements of atoms}
  \item{mc}{array of meet-complements of atoms}
  Class attribute, and a \code{warning} message when no substitution property found in induced inclusions.
}
\references{
Ardu, S. \emph{ASNET -- Algebraic and Statistical Network Analysis. User Manual}. University of Melbourne. 1995. (for the implementation algorithm)
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{decomp}}, \code{\link{cngr}}, \code{\link{pacnet}} 
}
\examples{
# create a partially ordered semigroup
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# semigroup of relations
S <- semigroup(arr)

# string relations and partial order
P <- strings(arr) |> 
  partial.order()

# perform the factorisation of PO S
fact(S, P)
}
\keyword{algebra}
\keyword{math}
