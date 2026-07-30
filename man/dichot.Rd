\name{dichot}
\alias{dichot}
\title{
Dichotomize Data with a Cutoff Value
}
\description{
Function to dichotomize the input data for the semigroup construction with a cutoff value.
}
\usage{
dichot(x, c = 1, diag)
}
\arguments{
  \item{x}{An array, matrix or other data in a numeric form.}
  \item{c}{A cutoff value to perform the dichotomization in \code{x}.}
  \item{diag}{(optional and logical, default \code{TRUE}) Should diagonals be included in output?}
}
\details{
This is a wrapper of the \code{\link[base:replace]{replace}} base function where in this case 
is aimed to specify a cutoff value for the dichotomization of the data. 
Values equal or higher to the cutoff are hence converted to \code{1}, while the others are set to \code{0}. 
Notice that the cutoff value in \code{c} can be any real number, and labels are preserved after the dichotomization..
}
\value{
Binary values of the input data with the same format as \code{x}.
}
%\references{} ~
%\author{} ~
%\note{} ~
%\section{Warning }{....} ~
\seealso{
\code{\link[base:replace]{replace}}, \code{\link{mnplx}}, \code{\link{semigroup}}.
}
\examples{
# create two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

# dichotomize it with a cutoff value of two
dichot(arr, c = 2)
}
\keyword{manip}
