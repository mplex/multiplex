\name{reduc}
\alias{reduc}
\title{
Reduce a matrix or array
}
\description{
Function to reduce a matrix or array with a given clustering vector
}
\usage{
reduc(x, clu, lbs = NULL, slbs = NULL, valued, row, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   a matrix or a three-dimensional array to be reduced
}
  \item{clu}{
   a vector with the class membership
}
  \item{lbs}{
   (optional) the labels to be used in the reduction
}
  \item{slbs}{
   (optional) the string labels to be used in the reduction
}
  \item{valued}{
   (logical) whether the reduction should preserve valued data?
  }
  \item{row}{
   (optional) the reduction by rows
}
  \item{col}{
   (optional) the reduction by columns
}
}
\details{
Given a partition, this function serves to reduce either a matrix representing e.g. a partial order structure. However, the reduction is also generalized to three-dimensional arrays representing multiple relations.
}
\value{
The reduced matrix or a reduced three-dimensional array of the input data according to the clustering information.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
Use \code{\link{decomp}} for the reduction of a semigroup object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cngr}}, \code{\link{rbox}}, \code{\link{decomp}}
}
\examples{
## scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## Reduce the multiplication table
reduc(s, clu=c(1,2,2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ array }
\keyword{cluster}
