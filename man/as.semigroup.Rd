\name{as.semigroup}
\alias{as.semigroup}
%
\title{
Coerce Relational System into a Semigroup Object
}
\description{
A generic function for coercing an \strong{R} object to a \code{\link{semigroup}} class.
}
\usage{
as.semigroup(x, gens = NA, lbs, numerical, edgeT)
}
%
\arguments{
  \item{x}{
	an array representing the semigroup
}
  \item{gens}{
	array or vector representing the semigroup generators
}
  \item{lbs}{
	(optional) label strings for the semigroup
}
  \item{numerical}{
	(optional and logical) should the semigroup have numerical format?
}
  \item{edgeT}{
	(optional and logical) is \sQuote{\code{x}} an edge table?
}

}
\details{
Because some of the routines in the \code{multiplex} package require an object of the \sQuote{\code{Semigroup}} class, 
this function produces this class object from an array representing the semigroup structure.
}
\value{
An object of the \sQuote{\code{Semigroup}} class
  \item{ord}{
  a number with the dimension of the semigroup
  }
  \item{st}{
  the strings, i.e. a vector of the unique relations
  }
  \item{gens}{
  the semigroup generators
}
  \item{S}{
  the multiplication table of the semigroup
  }
%% ...
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{semigroup}}, \code{\link{green.rel}}
}
\examples{
# create labeled multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

# set attributes to 's'
attr(s, "dimnames") <- list(1:3, 1:3)

# make a semigroup object
as.semigroup(s)
}
\keyword{algebra}
\keyword{math}
%\keyword{array}
