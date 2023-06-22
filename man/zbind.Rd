\name{zbind}
\alias{zbind}
\title{
Combine Arrays 
}
\description{
Combine multidimensional arrays.
}
\usage{
zbind(..., sort, force)
}
\arguments{
  \item{\dots}{
   One or more arrays with two or three dimensions
}
  \item{sort}{
   (optional and logical) sort array according to labels?
  }
  \item{force}{
   (optional and logical) force binding matrices with different order?
}
}
\details{
This function is for stacking two-dimensional arrays into a single three-dimensional object to represent a multivariate system structure. Both square and rectangular arrays are supported provided that the dimensions in the input are equal. The \code{dimnames} in the output correspond to the first array in the input, and a Warning message is given when these are NULL.
% This function represents an extension to both the \code{\link[base:cbind]{cbind}} and the \code{\link[base:rbind]{rbind}} functions, and it is especially useful in constructing a single object to represent a multivariate system structure. Both square and rectangular arrays are supported provided that the dimensions in the input are equal.
}
\value{
Usually a three dimensional array
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
Data frames should be transformed into arrays
}
\seealso{
\code{\link{mnplx}}, \code{\link{dichot}}, \code{\link{strings}}
}
\examples{
## Create the data: two sets with a pair of binary relations 
## among three elements
arr1 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .5, 3 ) )

arr2 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .5, 3 ) )

## bind the data sets
zbind(arr1, arr2)
}
\keyword{ manip }
\keyword{ array }
