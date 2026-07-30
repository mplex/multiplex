\name{green.rel}
\alias{green.rel}
\title{
Green's Relations of Abstract Semigroups
}
\description{
A function to produce the Green's relations of a semigroup object. 
}
\usage{
green.rel(x)
}
\arguments{
  \item{x}{A \dQuote{\code{Semigroup}} class object.}
}
\details{
Function \code{green.rel} produces the \emph{egg-box diagram} (Green, 1951) of an abstract semigroup \eqn{S}, 
which is the union of the left compatible \eqn{R} equivalence and 
the right compatible \eqn{L} equivalence classes that makes the \eqn{D}-class on \eqn{S}. 
Notice that some systems have the \eqn{D}-class equal to \eqn{S}.
}
\value{
A list object of \dQuote{\code{Green.Rels}} and \dQuote{\code{Semigroup}} class having:
  \item{S}{multiplication matrix of the abstract semigroup}
  \item{gens}{generator relations of \eqn{S}}
  \item{dim}{dimension of semigroup generators}
  \item{ord}{order of \eqn{S}}
  \item{st}{unique string relations}
  \item{clu}{list of vectors with clustering information of equivalence classes \eqn{R} and \eqn{L}}
  \item{R}{\eqn{R} equivalence classes}
  \item{L}{\eqn{L} equivalence classes}
  \item{D}{\eqn{D} equivalence classes}
}
\references{
Green, J. \dQuote{On the structure of semigroups,} \emph{Annals of Mathematics} 54(1), 163--172, 1951. \cr
Ostoic, JAR  \dQuote{Relational systems of transport network and provinces in ancient Rome,} in 
       \emph{Mathematics for social sciences and arts -- algebraic modeling}. Springer Nature. 2023.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{semigroup}}, \code{\link{diagram}}, \code{\link{as.semigroup}}, 
\code{\link{edgeT}}, \code{\link{wordT}}, \code{\link{fact}}, \code{\link{cngr}}, \code{\link{decomp}}  
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )
# optional string labels
dimnames(arr)[[3]] <- list("n", "m")

# look at the Green's relations of its semigroup
semigroup(arr) |> 
  green.rel()
}
\keyword{algebra}
\keyword{math}
