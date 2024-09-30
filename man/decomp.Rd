\name{decomp}
\alias{decomp}
\title{
Decomposition of a Semigroup Structure
}
\description{
A function to perform the decomposition of a semigroup structure
}
\usage{
decomp(S, pr, type = c("mca", "pi", "at", "cc"), reduc, fac, force)
}
%
\arguments{
  \item{S}{  
     an object of a \sQuote{\code{Semigroup}} class
}
  \item{pr}{  
     either an object of a \sQuote{\code{Congruence}} class or an object of a \sQuote{\code{Pi.rels}} class
}
  \item{type}{  
     type of decomposition; ie. the reduction is based on 
  \itemize{ 
    \item \code{mca} meet-complements of atoms in the \sQuote{\code{Pi.rels}} class
    \item \code{pi} \eqn{\pi}-relations in the \sQuote{\code{Pi.rels}} class
    \item \code{at} atoms
    \item \code{cc} congruence classes
  }
}
  \item{reduc}{  
     (optional and logical) does the return object should include the reduced structures?
}
  \item{fac}{  
     (optional) the factor that should be decomposed
}
  \item{force}{  
     (optional and logical) force further reduction of the semigroup when \code{S} has \code{NA}s? (see details)
}
}
\details{
The \code{decomp} function is a reduction form of an algebraic structure like the semigroup that verifies which of the class members in the system are congruent to each other. 
The decomposed object then is made of congruent elements, which form part of the lattice of congruence classes in the algebraic structure. 
In case that the input data comes from the Pacnet program, then such elements are in form of \eqn{\pi}-relations or the  meet-complements of the atoms; otherwise these are simply equivalent elements satisfying the substitution property.

Sometimes a \sQuote{\code{Semigroup}} class object contains not available data in the multiplication table, typically when it is an image from the \code{\link{fact}} function. 
In such case, it is possible to perform a reduction of the semigroup structure with the \code{force} option, which performs additional equations to the string relations in order to get rid of \code{NA}s in the semigroup data.
}
\value{
An object of \sQuote{\code{Decomp}} class having:
  \item{clu }{vector with the class membership}
  \item{eq }{the equations in the decomposition}
  \item{IM }{(optional) the image matrices}
  \item{PO }{(optional) the partial order table}
  \item{ord}{(optional) a vector with the order of the image matrices}
%% ...
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.

Hartmanis, J. and R.E. Stearns \emph{Algebraic Structure Theory of Sequential Machines}. Prentice-Hall. 1966.
}
\author{
Antonio Rivero Ostoic
}
\note{
Reduction of the partial order table should be made by the \code{\link{reduc}} function.
}

%

\seealso{
\code{\link{fact}}, \code{\link{cngr}}, \code{\link{reduc}}, \code{\link{pi.rels}}, 
\code{\link{semigroup}}, \code{\link{partial.order}}, \code{\link{green.rel}}.
}
%\examples{
%
%}
%
\keyword{algebra}
\keyword{math}
\keyword{array}
\keyword{cluster}
