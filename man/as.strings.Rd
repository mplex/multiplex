\name{as.strings}
\alias{as.strings}
\title{
Coerce Object into a Strings Class
}
\description{
A generic function for coercing an \R object into a \dQuote{\code{Strings}} class.
}
\usage{
as.strings(x, lbs)
}
\arguments{
  \item{x}{An array, usually with three dimensions of stacked matrices where the multiple relations are placed.}
  \item{lbs}{(optional) Labels of strings.}
}
\details{
Use this function to prepare an object for establishing the partial order of relation strings by converting it to class \dQuote{\code{Strings}}. 
It returns as well the number of unique relations in the semigroup represented by the dimensions in \code{x}.
}
\value{
The input array in \code{x} as an object of \dQuote{\code{Strings}} class
  \item{wt}{word tables}
  \item{ord}{dimension in \code{x}}
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link{strings}}, \code{\link{partial.order}}, \code{\link{zbind}}
}
\examples{
# create the data: two sets with a pair of binary relations among 
# three elements
arr1 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )

arr2 <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )

# bind the data sets
arrs <- zbind(arr1, arr2)

# make the data a strings object
as.strings(arrs)
}
\keyword{manip}
\keyword{classes}
\keyword{misc}
