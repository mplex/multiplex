\name{fact}
\alias{fact}
%
\title{
Factorisation of Semigroup Structures
}
\description{
A function to decompose partially ordered semigroups
}
\usage{
fact(S, P, uniq = TRUE, fac, atoms, mca, atmc, patm, k)
}
\arguments{
  \item{S}{
semigroup object
}
  \item{P}{
partial order structure associated to \code{S}
}
  \item{uniq}{
(logical) whether factorisation should include unique induced inclusions
}
  \item{fac}{
`factor' to be factorised, in case that input factorised partially ordered structures
}
  \item{atoms}{
(logical) whether or not include in output atoms
}
  \item{mca}{
(logical) whether or not include in output meet-complements of atoms
}
  \item{atmc}{
(logical) whether or not include in output atoms' meet-complements
}
  \item{patm}{
(logical) whether or not include in output potential atoms
}
  \item{k}{
(for \code{patm}) length of induced inclusion
}
}
\details{
The factorisation is part of decomposition for partially ordered semigroups, and
function \code{fact} allows to obtain elements generated in this process. 
}
\value{
An object of \sQuote{\code{Ind.incl}} class having:
  \item{po}{partial order table}
  \item{iin}{list of induced inclusions pairwise listed}
  \item{niin}{length of induced inclusions}
  \item{patm}{(for \code{patm}) a vector with potential atoms}
  \item{atm}{vector with atoms}
  \item{atmc}{(for \code{atmc}) array with meet-complements of atoms}
  \item{mc}{array of meet-complements of atoms}
  \item{note}{(if needed) induced inclusions without substitution property}
%% ...=iin,=ncomp,=patoms,=atm,atmc=mcl,mc=mmcs,note=znote
}
\references{
Ardu, S. \emph{ASNET -- Algebraic and Statistical Network Analysis. User Manual}. University of Melbourne. 1995.
}
\author{
Antonio Rivero Ostoic (based on the algorithm described in Ardu, 1995)
}
\note{
Data objects imported with \code{\link{pacnet}} are compatible for further factorisation. 
}
%
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
