\name{strings}
\alias{strings}
\title{
Strings of Relations
}
\description{
Function to obtain labels of unique relations, and generators and compound relations of a complete semigroup structure.
}
\usage{
strings(x, equat = FALSE, k = 2, smpl, valued)
}
\arguments{
  \item{x}{Array with three dimensions of stacked matrices with generating relations.}
  \item{equat}{(logical) Include equations in output?}
  \item{k}{Length of strings in equations.}
  \item{smpl}{(optional and logical) Simplify string relations for output?}
  \item{valued}{(optional and logical) Valued format in strings?}
}
\details{
The strings are the unique relations, which constitute the elements of the complete semigroup. 
These are both the generators and the compound relations after applying the Axiom of Quality, which means that even some generators can be disregarded.

This function is especially useful to construct the partial order of relations and to establish the set of equations in the relational structure.

The maximum length of the strings in the equations is currently \eqn{4}.
}
\value{
An object of \sQuote{\code{Strings}} class.
  \item{wt}{Generators and compound relations}
  \item{ord}{Order of structure}
  \item{st}{Labels of unique relations}
  \item{equat}{Equations among strings relations}
}
\references{
Boorman, S.A. and H.C. White, \dQuote{Social Structure from Multiple Networks. II. Role Structures.} \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.
}
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{partial.order}}, \code{\link{semigroup}}.
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

# get string relations
strings(arr)
}
\keyword{algebra}
\keyword{array}
