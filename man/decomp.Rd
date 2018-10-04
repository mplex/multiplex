\name{decomp}
\alias{decomp}
\title{
Decomposition of a Semigroup Structure
}
\description{
A function to perform the decomposition of a semigroup structure
}
\usage{
decomp(S, pr, type = c("mc", "pi", "at", "cc"), reduc, fac)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{  
     an object of a `\code{Semigroup}' class
}
  \item{pr}{  
     either an object of a `\code{Congruence}' class or an object of a `\code{Pi.rels}' class
}
  \item{type}{  
     whether the reduction is based on a a congruence class (option \code{"cc"}) or rather on a \eqn{\pi}-relation, atoms, or a meet-complement in the `\code{Pi.rels}' class
}
  \item{reduc}{  
     (optional and logical) does the return object should include the reduced structures?
}
  \item{fac}{  
     (optional) the factor that should be decomposed
}
}
\details{
The \code{decomp} function is a reduction form of an algebraic structure like the semigroup that verifies which of the class members in the system are congruent to each other. The decomposed object then is made of congruent elements, which form part of the lattice of congruence classes in the algebraic structure. In case that the input data comes from the Pacnet program, then such elements are in form of \eqn{\pi}-relations or the  meet-complements of the atoms; otherwise these are simply equivalent elements satisfying the substitution property.
}
\value{
An object of `\code{Decomp}' class having:
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fact}}, \code{\link{cngr}}, \code{\link{reduc}}, \code{\link{pi.rels}}, \code{\link{semigroup}}, \code{\link{partial.order}}
}
%\examples{
%
%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ algebra }
\keyword{ math }
\keyword{ array }
\keyword{cluster}
