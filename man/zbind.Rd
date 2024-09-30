\name{zbind}
\alias{zbind}
\title{
Bind Matrices and Multidimensional Arrays 
}
\description{
A function to combine or bind matrices and multidimensional arrays.
}
\usage{
zbind(..., sort, force)
}
\arguments{
  \item{\dots}{
   One or more arrays having two or three dimensions
}
  \item{sort}{
   (optional and logical) sort array according to labels?
  }
  \item{force}{
   (optional and logical) force binding arrays with different dimensions?
}
}
\details{
Function \code{zbind} is for stacking for instance two-dimensional arrays into a single three-dimensional object to represent a multivariate system structure. 
Both square and rectangular arrays are supported provided that the dimensions in the input are equal, and data frames should be transformed into arrays for the binding. 
The \code{dimnames} in the array output correspond to the names of the first array in the input, and a \code{warning} message is given when the dimnames are \code{NULL}. 
% This function represents an extension to both the \code{\link[base:cbind]{cbind}} and the \code{\link[base:rbind]{rbind}} functions, and it is especially useful in constructing a single object to represent a multivariate system structure. Both square and rectangular arrays are supported provided that the dimensions in the input are equal.
}
\value{
Usually a three dimensional array
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
\note{
Argument \code{force} must be set to \code{TRUE} for matrices and arrays with different dimensions, and a \code{message} is given.
}
\section{Warning}{
Arrays without dimnames are not fully supported.
}
\seealso{
\code{\link{mnplx}}, \code{\link{dichot}}, \code{\link{strings}}
}
\examples{
# create the data: two sets with a pair of binary relations 
# among three elements
arr1 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .5, 3 ) )

arr2 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .5, 3 ) )

# bind the data sets
zbind(arr1, arr2)
}
\keyword{manip}
\keyword{array}
