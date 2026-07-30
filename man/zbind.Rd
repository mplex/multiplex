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
  \item{\dots}{One or more arrays having two or three dimensions.}
  \item{sort}{(optional and logical) Sort array in output according to labels?}
  \item{force}{(optional and logical) Force binding arrays with different dimensions?}
}
\details{
Function \code{zbind} is for stacking for instance two-dimensional arrays into a single three-dimensional object to represent a multivariate system structure. 
Both square and rectangular arrays are supported provided that the dimensions in the input are equal, and data frames should be transformed into arrays for the binding. 

The \code{dimnames} in the array output correspond to the names of the first array in the input, and a \code{warning} message is given when the dimnames are \code{NULL}. 

Argument \code{force} must be set to \code{TRUE} for matrices and arrays with different dimensions, and a \code{message} is given.
}
\value{
Mostly a three dimensional array.
}
%\references{} ~
%\author{} ~
%\note{} ~
\section{Warning}{
Arrays without dimnames are not fully supported.
}
\seealso{
\code{\link{mnplx}}, \code{\link{dichot}}, \code{\link{strings}}
}
\examples{
# create two sets with two binary relations among three elements
arr1 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .9, 3 ) )

arr2 <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
        c(3, 3, 2) ) > .5, 3 ) )

# bind sets of data
zbind(arr1, arr2)
}
\keyword{manip}
\keyword{array}
