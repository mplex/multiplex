\name{pi.rels}
\alias{pi.rels}
\title{
\eqn{\pi}-Relations
}
\description{
A function to establish the \eqn{\pi}-relations of a partially ordered structure comming from a \sQuote{\code{Pacnet}} class
}
\usage{
pi.rels(x, po.incl, vc, po)
}
%
\arguments{
  \item{x}{
  an object of a \sQuote{\code{Pacnet}} class
}
  \item{po.incl}{
  (optional and logical) should the partial order be included in the outcome?
}
  \item{vc}{
  (optional) vector of the induced inclusions to be computed
}
  \item{po}{
  (optional) the partial order structure
}
}
\details{
This function process the outcome of the \bold{Pacnet} report by adding induced inclusions to partial order, the minimal element of the lattice of congruence relations. 
Such type of structure serves for the decomposition of a partially ordered semigroup structure.
}
\value{
An object of the \sQuote{\code{Pi.rels}} class

  \item{pi}{the \eqn{\pi}-relations, eventually with the partial order}
  \item{mca}{the meet-complements of atoms}
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
%
\seealso{
\code{\link{pacnet}}, \code{\link{decomp}}, \code{\link{semigroup}}
}
%\examples{
%}
%
\keyword{algebra}
\keyword{math}
