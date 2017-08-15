\name{is.mc}
\alias{is.mc}
\title{
Are Two \eqn{\pi}-relations Meet Complements?
}
\description{
Function to find whether two \eqn{\pi}-relations are Meet Complement of a given an Atom.
}
\usage{
is.mc(B, C, A, ord = NULL)
}
\arguments{
  \item{B}{  one \eqn{\pi}-relation
}
  \item{C}{  another \eqn{\pi}-relation
}
  \item{A}{  the Atom
}
  \item{ord}{  (optional) the order of the semigroup
}
}
\details{
Two congruent or \eqn{\pi}-relations are meet complement to a join irreducible element of the lattice of congruence relations whenever their greatest lower bound is the minimal element in the congruence lattice.
}
\value{
A logical indicating whether the input data are meet-complements of the Atom.
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
\note{
The congruence relations must be in a 'list' fomat.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{iinc}}, \code{\link{cngr}}, \code{\link{decomp}}, \code{\link{pi.rels}}
}
%\examples{
%
%}
\keyword{ algebra }

