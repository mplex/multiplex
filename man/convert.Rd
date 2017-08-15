\name{convert}
\alias{convert}
\title{
Convert Format of a Semigroup Object
}
\description{
A function to convert an object representing a semigroup of relations from Numerical to Symbolic format and vice versa.
}
\usage{
convert(x, lbs = NULL, SemigroupClass = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   an object from the `\code{Semigroup}' class.
}
  \item{lbs}{
   (optional) the labels to be used in the conversion
}
  \item{SemigroupClass}{
   (logical) whether the output should be a `\code{Semigroup}' class
}
}
\details{
In order to avoid the construction of the semigroup from primitive relations (since it might take a relative long time), this function is aimed to convert the semigroup structure in a fast way from a numerical to a symbolic form or vice versa.
}
\value{
An array representing the semigroup of relations or an object of the `\code{Semigroup}' class.

}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
In case that the input data is in `\code{symbolic}' form, then \code{labels} is ignored, and a `Warning' message is printed in the output.
}

\seealso{
\code{\link{semigroup}}, \code{\link{as.semigroup}}, \code{\link{relabel}}
}
\examples{
## Create the multiplication table
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## make a semigroup object with labels
S <- as.semigroup(s, lbs=c('n', 'm', 'ñ'))

## convert the semigroup to a symbolic form
convert(S)
}
\keyword{ manip }
\keyword{ array }
