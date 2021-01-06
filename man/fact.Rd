\name{fact}
\alias{fact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Factorization of semigroup structures
}
\description{
A function to decompose partially ordered semigroups
}
\usage{
fact(S, P, uniq = TRUE, fac, atoms, mca, atmc, patm, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
a semigroup object, \code{S}
}
  \item{P}{
a partial order structure associated to \code{S}
}
  \item{uniq}{
(logical) whether the factorization include the unique induced inclusions
}
  \item{fac}{
(integer) the `factor' to be factorized (see details)
}
  \item{atoms}{
(logical) whether or not include the atoms in the output
}
  \item{mca}{
(logical) whether or not include the meet-complements of atoms in the output
}
  \item{atmc}{
(logical) whether or not include the atoms'  meet-complements in the output
}
  \item{patm}{
(logical) whether or not include the potential atoms in the output
}
  \item{k}{
the length of the induced inclusion (only relevant if \code{patm} is activated)
}
}
\details{
The factorization is part of the decomposition process for partially ordered semigroups, which means that there are two objects in the input. 
The induced inclusions are additions to the partial order and, depending on the needs; it is possible to customize the rest of the output. 
Atoms and meet-complement of these are useful for the decomposition through the \code{\link{decomp}} that produce a number of reduced structures or `factors'. 
Argument \code{fac} allows choosing a factor for a progressive factorization.

}
\value{
An object of \code{"Ind.incl"} class having:
  \item{po}{the partial order table}
  \item{iin}{list of induced inclusions pairwise listed}
  \item{niin}{length of the induced inclusion}
  \item{patm}{(optional) a vector with the potential atoms}
  \item{atm}{(optional) a vector with the atoms}
  \item{atmc}{(optional) array with meet-complements of atoms}
  \item{mca}{(optional) meet-complements of atoms}
  \item{note}{(optional) induced inclusions without the substitution property}
%% ...=iin,=ncomp,=patoms,=atm,atmc=mcl,mc=mmcs,note=znote
}
\references{
Pattison, P. and Bartlett, W., `A factorization procedure for finite algebras,' \emph{Journal of Mathematical Psychology} 25, 51-81. 1982.

Ardu, S. \emph{ASNET -- Algebraic and Statistical Network Analysis. User Manual}. University of Melbourne. 1995.

}
\author{
Antonio Rivero Ostoic (based on the algorithm described in Ardu, 1995)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{decomp}}, \code{\link{cngr}}, \code{\link{pacnet}} 
}
\examples{
## Create a partially ordered semigroup
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )
# semigroup
S <- semigroup(arr)
# string relations
St <- strings(arr)
# partial order
P <- partial.order(St)

## Perform the factorisation of PO S
fact(S, P)
}
\keyword{ algebra }
\keyword{ math }
