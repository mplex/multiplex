\name{green.rel}
\alias{green.rel}
\title{
Green's Relations of Abstract Semigroups
}
\description{
A function to produce the Green's relations of a semigroup object. 
}
\usage{
green.rel(S)
}
\arguments{
  \item{S}{
     an object of a `\code{Semigroup}' class
}
}
\details{
Function \code{green.rel} produces the \emph{egg-box diagram} (Green, 1951) of an abstract semigroup \eqn{S}, 
which is the union of the left compatible \eqn{R} equivalence and 
the right compatible \eqn{L} equivalence classes that makes the \eqn{D}-class on \eqn{S}.
}
\value{
A list with the abstract semigroup, clustering of equivalence classes, and egg-box diagram that 
are separated by \code{|} and \code{-}.

  \item{S}{multiplication matrix of the input semigroup}
  \item{ord}{dimension of the semigroup}
  \item{st}{vector of the unique string relations}
  \item{clu}{list of vectors with clustering information for \eqn{R} and \eqn{L} classes}
  \item{R}{\eqn{R} equivalence classes}
  \item{L}{\eqn{L} equivalence classes}
  \item{D}{\eqn{D} equivalence classes}
}
\references{
Green, J. ``On the structure of semigroups'', \emph{Annals of Mathematics} 54(1), 163--172, 1951.

Ostoic, JAR  ``Relational systems of transport network and provinces in ancient Rome,'' in 
       \emph{Mathematics for social sciences and arts -- algebraic modeling}. Springer Nature. 2023.
}
\author{
Antonio Rivero Ostoic
}
\note{
Some systems have the \eqn{D}-class equal to \eqn{S}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semigroup}}, \code{\link{diagram}}, \code{\link{as.semigroup}}, 
\code{\link{edgeT}}, \code{\link{wordT}}, \code{\link{fact}}, \code{\link{cngr}}, \code{\link{decomp}}  
}
\examples{
## Create the data: 2 binary relations among 3 elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )
## optional string labels
dimnames(arr)[[3]] <- list("n", "m")

## look at the semigroup and its Green's relations
semigroup(arr) |> 
  green.rel()
}
\keyword{ algebra }
\keyword{ math }
