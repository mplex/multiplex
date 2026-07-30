\name{semigroup}
\alias{semigroup}
\title{
Semigroup of Relations for Multiple Networks
}
\description{
Function to create the complete semigroup of multiple relations, where the multiplication table can be specified with either a numerical or a symbolic form.
}
\usage{
semigroup(x, type = c("numerical", "symbolic"), cmps, smpl, valued)
}
\arguments{
  \item{x}{Array with three dimensions of stacked matrices with generating relations.}
  \item{type}{
   Semigroup multiplication table to return: 
  \itemize{ 
    \item \code{numerical} format (default)
    \item \code{symbolic} format
  }}
  \item{cmps}{(optional and logical) Include composite matrices in output?}
  \item{smpl}{(optional and logical) Simplify strings of relations?}
  \item{valued}{(optional and logical) Is the semigroup of a valued format?}
}
\details{
A multiple relation can be defined by square matrices of \code{0}s and \code{1}s indicating the presence and absence of ties among a set of actors. 
If there is more than one relation type, the matrices must preserve the label ordering of its elements and stacked into an object array in order to be effectively applied to this function.

The semigroup, which is an algebraic structure having a set with an associative operation on it, is calculated considering binary matrices only. 
This means that if the provided matrices are valued, the function will dichotomise the input data automatically.
Values higher or equal to a unit are converted to one; otherwise they are set to zero. 
For other values use function \code{\link{dichot}} to specify a cutoff value for the dichotomization. 

Semigroup structures for valued relations apply the max min operation in the composition of generators and strings, and depend on function \code{\link{cscl}}. 
}
\value{
A \sQuote{\code{Semigroup}} class object with items:
  \item{gens}{Array with generator relations}
  \item{cmps}{Array with the unique compound relations}
  \item{ord}{Dimension of the semigroup}
  \item{st}{Vector of unique string relations}
  \item{S}{Multiplication matrix with semigroup of relations (numerical or symbolic)}
If the specified type is \code{numerical}, then a matrix of semigroup values is given; otherwise the values is returned as a data frame with the strings of the semigroup.
}
\references{
Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976. \cr
Boyd, J.P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991. \cr
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
%\author{} ~
%\note{} ~
\section{Warning}{
Computing for the semigroup construction can take very long time for medium size or bigger sets (e.g. having more than 4 relation types).
}
\seealso{
\code{\link{green.rel}}, \code{\link{strings}}, \code{\link{edgeT}}, \code{\link{wordT}}, \code{\link{cngr}}, \code{\link{cscl}}
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# optionally put labels
dimnames(arr)[[3]] <- list("n", "m")

# look at the semigroup with numerical format
semigroup(arr)
}
\keyword{algebra}
\keyword{math}
