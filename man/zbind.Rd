\name{zbind}
\alias{zbind}
\title{
Combine multidimensional arrays.
}
\description{
A function to combine arrays with equal or unequal dimensions.
}
\usage{
zbind(..., sort, force)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
arrays to bind
  }
  \item{sort}{
(optional and logical) sort dimnames in output array?
  }
  \item{force}{
(optional and logical) force bind for unequal dimensions arrays?
  }
}
\details{
This function is for stacking two- or three-dimensional arrays into a single object to represent a multivariate system structure. 
Both square and rectangular arrays are supported provided that the dimensions in the input are equal, but data frames need to be transformed into arrays. 
The \code{dimnames} in the output correspond to the first array in the input, and a \code{Warning} message is given when these labels are \code{NULL}.

Activate \code{force} when two or more arrays have different dimensions to generate a three-dimensional array that includes all elements. 
This is the default option that is needed for working with temporal or dynamic networks with changing size and order.
}
\value{
Usually a three-dimensional array representing a multiplex network.
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
This routine is an extension of both \code{\link[base:cbind]{cbind}} and \code{rbind} functions. 
}
\seealso{
\code{\link{mnplx}}, \code{\link{dichot}}, \code{\link{strings}}, \code{\link[base:cbind]{cbind}}
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
