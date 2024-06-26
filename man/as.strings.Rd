\name{as.strings}
\alias{as.strings}
%
\title{
Coerce an Object to a Strings Class
}
\description{
A generic function for coercing an R object to a \code{Rel.Strings} class.
}
\usage{
as.strings(x, lbs = NULL)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{lbs}{
   (optional) the labels of the strings
  }
}
\details{
This function is useful to proceed with the establishment of the partial order in the strings of relations where the object should be of a `\code{Strings}' class.
}
\value{
An object of `\code{Strings}' class
  \item{wt}{
  the word tables
  }
  \item{ord}{
  the number of unique relations in the semigroup
  }
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%}
\seealso{
\code{\link{strings}}, \code{\link{partial.order}}, \code{\link{zbind}}
}
\examples{
## Create the data: two sets with a pair of binary relations among 
## three elements
arr1 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )

arr2 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )

## bind the data sets
arrs <- zbind(arr1, arr2)

## make the data a strings object
as.strings(arrs)

}
\keyword{algebra}
\keyword{array}
