\name{pi.rels}
\alias{pi.rels}
\title{
\eqn{\pi}-Relations
}
\description{
A function to produce the \eqn{\pi}-relations of a partially ordered structure.
}
\usage{
pi.rels(x, po.incl, vc, po)
}
\arguments{
  \item{x}{An object of a \dQuote{\code{Ind.incl}} or \dQuote{\code{Pacnet}} class.}
  \item{po.incl}{(optional and logical) Include partial order in outcome?}
  \item{vc}{(optional) Vector with induced inclusions to compute.}
  \item{po}{(optional) Matrix with partial order associated to induced inclusions.}
}
\details{
The totality of \eqn{\pi}-relations are represented with an array format, and the arrangement of meet-complements in 
this array are congruences that serve for the decomposition of a partially ordered semigroup structure.

Induced inclusions to the partial order in \code{x} as a \dQuote{\code{Ind.incl}} class object are produced by the \code{\link{fact}} function, 
while induced inclusions as a \dQuote{\code{Pacnet}} class object comes from the outcome of a \proglang{Pacnet} log-report. 
With either case, \eqn{\pi}-relations are derived from the induced inclusions in a factorization process of a partially ordered semigroup.

A visualization of the \eqn{\pi}-relations partial order arrangement with function \code{\link{diagram}} is feasible provided that 
the resulted \dQuote{\code{Pi.rels}} class object computes a \dQuote{\code{Pi.rels}} class with the \code{type} option \code{"pi.rels"} in function \code{\link{partial.order}}. 
In such case, including the partial order with \code{po.incl} and a addition in \code{po} is a good idea since the partial order is the minimal element of the lattice of congruence relations. 

Producing the partial order structure of a set of \eqn{\pi}-relations can be resource consuming and for exploring and other purposes, with \code{vc} option 
one can limit the amount of induced inclusions to compute.
}
\value{
An object of the \dQuote{\code{Pi.rels}} class with:
  \item{pi}{\eqn{\pi}-relations, eventually with partial order}
  \item{at}{atoms}
  \item{mca}{meet-complements of atoms}
  \item{po}{partial order (if included)}
}
\references{
Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{fact}}, \code{\link{decomp}}, \code{\link{partial.order}}, \code{\link{diagram}}, \code{\link{semigroup}}, \code{\link{pacnet}}
}
%\examples{
%}
%
\keyword{algebra}
\keyword{math}
