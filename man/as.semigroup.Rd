\name{as.semigroup}
\alias{as.semigroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coerce to a Semigroup Object
}
\description{
A generic function for coercing an R object to a \code{\link{semigroup}} class.
}
\usage{
as.semigroup(x, lbs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	an array representing the semigroup
}
  \item{lbs}{
	(optional) label strings for the semigroup
}
}
\details{
Since many of the functions in the \code{multiplex} package require an object of the `\code{Semigroup}' class, this function produces this class object from an array representing the semigroup structure.
}
\value{
An object of the `\code{Semigroup}' class
  \item{ord}{
  a number with the dimension of the semigroup
  }
  \item{st}{
  the strings, i.e. a vector of the unique relations
  }
  \item{S}{
  the semigroup of relations
  }
%% ...
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
\note{
In this function the input object typically comes from the \code{\link{convert}} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{convert}}, \code{\link{semigroup}}
}
\examples{
## scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## make a semigroup object
as.semigroup(s)

}
\keyword{ algebra }
\keyword{ math }
%\keyword{ array }
