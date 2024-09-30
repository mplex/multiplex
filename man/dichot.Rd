\name{dichot}
\alias{dichot}
%
\title{
Dichotomize Data with a Cutoff Value
}
\description{
Function to dichotomize the input data for the semigroup construction with a cutoff value.
}
\usage{
dichot(x, c = 1, diag)
}
%
\arguments{
  \item{x}{
   some data in a numeric form (usually arrays)
}
  \item{c}{
   the cutoff value to perform the dichotomization (default \code{1})
}
  \item{diag}{
  (optional and logical) whether or not the diagonals should be included (default \code{TRUE})
}
}
\details{
This is a convenient function (or wrapper if you like) of the \code{\link[base:replace]{replace}} function. 
In this case, the function is aimed to specify a cutoff value for the dichotomization of the data where the 
values equal or higher to the cutoff are converted to one, while the others are set to zero. 
The cutoff value in \code{c} can be any real number.
}
\value{
Binary values of the input data.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
Labels are preserved after the dichotomization.
}
%
\seealso{
\code{\link[base:replace]{replace}}, \code{\link{prev}}, \code{\link{semigroup}}.
}
\examples{
# create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

# dichotomize it with a cutoff value of two
dichot(arr, c = 2)
}
\keyword{manip}
%
