\name{reduc}
\alias{reduc}
\title{
Reduce Matrices or Arrays
}
\description{
Function to reduce a matrix or an array with a given clustering vector.
}
\usage{
reduc(x, clu, lbs, slbs, valued, row, col)
}
\arguments{
  \item{x}{Matrix or a three-dimensional array to be reduced.}
  \item{clu}{Vector with class membership in \code{x}.}
  \item{lbs}{(optional) Vector with labels for the reduced structure.}
  \item{slbs}{(optional) Vector with string labels in the reduced structure.}
  \item{valued}{(optional and logical) Should the reduction preserve valued data?}
  \item{row}{(optional and logical) Reduction by rows.}
  \item{col}{(optional and logical) Reduction by columns.}
}
\details{
Given a partition, this function serves to reduce either a matrix representing e.g. a partial order structure. 
However, the reduction is also generalized to three-dimensional arrays representing multiple relations or 
affiliation networks with either \code{row} or \code{col}. 

Notice that the \dQuote{reduction} of a semigroup of \code{x} is made through a decomposition process that is computed 
with function \code{\link{decomp}}.
}
\value{
A reduced matrix or a reduced three-dimensional array of the input data according to the clustering information.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{cngr}}, \code{\link{rbox}}, \code{\link{decomp}}
}
\examples{
# scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

# reduce the multiplication table
s |> reduc(clu=c(1,2,2))
}
\keyword{manip}
\keyword{array}
\keyword{cluster}
