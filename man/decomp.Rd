\name{decomp}
\alias{decomp}
\title{
Decomposition of a Semigroup Structure
}
\description{
A function to perform the decomposition of a semigroup structure.
}
\usage{
decomp(S, pr, type = c("mca", "pi", "at", "cc"), reduc, fac)
}
\arguments{
  \item{S}{A \dQuote{\code{Semigroup}} class object.}
  \item{pr}{Either an object of a \dQuote{\code{Congruence}} class or an object of a \dQuote{\code{Pi.rels}} class.}
  \item{type}{Type of decomposition on \code{S} where the reduction is based on: 
  \itemize{ 
    \item \code{mca} meet-complements of atoms in the \dQuote{\code{Pi.rels}} class
    \item \code{pi} \eqn{\pi}-relations in the \dQuote{\code{Pi.rels}} class
    \item \code{at} atoms
    \item \code{cc} congruence classes.
  }}
  \item{reduc}{(optional and logical) Should the returned list include the reduced structures?}
  \item{fac}{(optional) Factor subject to decomposition.}
}
\details{
The \code{decomp} function reduces an algebraic structure, such as a semigroup, to determine which class members in the system are congruent. 
The resulting object consists of congruent elements that belong to the lattice of congruence classes in the structure. 
If the input data comes from the \proglang{Pacnet} program, these elements take the form of \eqn{\pi}-relations or meet-complements of atoms; 
otherwise, they are equivalent elements that satisfy the substitution property.

A \dQuote{\code{Semigroup}} class object may contain unavailable values in its multiplication table, typically when it is produced as an image by the \code{\link{fact}} function. 
In such cases, the semigroup can be reduced with the \code{force} option, which adds equations to the string relations to eliminate \code{NA} values in the semigroup data.

Use the \code{\link{reduc}} function to reduce the partial order table.
}
\value{
A list object of \dQuote{\code{Decomp}} class having:
  \item{clu }{vector with class membership}
  \item{eq }{equations in decomposition}
  \item{IM }{(optional) image matrices}
  \item{PO }{(optional) partial order table}
  \item{ord}{(optional) vector with order of image matrices}
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993. \cr
Hartmanis, J. and R.E. Stearns \emph{Algebraic Structure Theory of Sequential Machines}. Prentice-Hall. 1966.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{fact}}, \code{\link{cngr}}, \code{\link{reduc}}, \code{\link{pi.rels}}, 
\code{\link{semigroup}}, \code{\link{partial.order}}, \code{\link{green.rel}}.
}
%\examples{} ~
\keyword{algebra}
\keyword{math}
\keyword{array}
\keyword{cluster}
