\name{semigroup}
\alias{semigroup}
%
\title{
Constructing the Semigroup of Relations of Multiple Networks
}
\description{
Function to create the complete semigroup of multiple relations, where the multiplication table can be specified with either a numerical or a symbolic form.
}
\usage{
semigroup(x, type = c("numerical", "symbolic"), cmps, smpl, valued)
}
%
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed
}
  \item{type}{
   semigroup multiplication table to be returned 
  \itemize{ 
    \item \code{numerical} for a numerical format (default)
    \item \code{symbolic} for a symbolic format
  }
}
  \item{cmps}{
   (optional and logical) whether the composite matrices should be also given in the output
}
  \item{smpl}{
   (optional and logical) whether to simplify or not the strings of relations
}
  \item{valued}{
   (logical) whether the semigroup should be with a valued format
  }
}
\details{
A multiple relation can be defined by square matrices of \code{0}s and \code{1}s indicating the presence and absence of ties among a set of actors. 
If there is more than one relation type, the matrices must preserve the label ordering of its elements and stacked into an object array in order to be effectively applied to this function.

The semigroup, which is an algebraic structure having a set with an associative operation on it, is calculated considering binary matrices only. 
This means that if the provided matrices are valued, the function will dichotomise the input data automatically.
Values higher or equal to a unit are converted to one; otherwise they are set to zero. 
If not happy, use function \code{\link{dichot}} to specify a cutoff value for the dichotomization. 

Semigroup structures for valued relations apply the max min operation in the composition of generators and strings. 
}
\value{
An object of \sQuote{\code{Semigroup}} class.
The items included are:
  \item{gens}{
  array with generator relations
}
  \item{cmps}{
  array with the unique compound relations
}
  \item{ord}{
  dimension of the semigroup
}
  \item{st}{
  vector of the unique string relations
}
  \item{S}{
  multiplication matrix with semigroup of relations (see below)
}

If the specified type is \code{numerical}, then a matrix of semigroup values is given, otherwise the values is returned as a data frame with the strings of the semigroup.
}
\references{
Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.

Boyd, J.P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991.

Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
\note{
It is recommendable to perform the function \code{prev} before attempting to construct the semigroup, unless the input data has few dimensions.
}

\section{Warning}{
For medium size or bigger sets (having e.g. more the 4 relation types), the semigroup construction could take a long time.
}

\seealso{
\code{\link{green.rel}}, \code{\link{prev}}, \code{\link{strings}}, \code{\link{edgeT}}, \code{\link{wordT}}, \code{\link{cngr}}.
}

\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 1 ) )

# optional: put labels
dimnames(arr)[[3]] <- list("n", "m")

# look at the semigroup with numerical format
semigroup(arr)
}
%
\keyword{algebra}
\keyword{math}
%